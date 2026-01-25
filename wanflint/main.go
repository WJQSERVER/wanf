package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"os"
	"runtime"
	"runtime/debug"
	"sync"

	"github.com/WJQSERVER/wanf"

	"github.com/go-json-experiment/json"
	"github.com/go-json-experiment/json/jsontext"
)

const usage = `wanflint: a tool for linting and formatting WANF files.

Usage:
  wanflint <command> [arguments]

Commands:
  lint [path ...]   lint files and report issues
  fmt [path ...]    format files
  info              display information about wanflint
`

func main() {
	if len(os.Args) < 2 {
		fmt.Fprint(os.Stderr, usage)
		os.Exit(1)
	}

	lintCmd := flag.NewFlagSet("lint", flag.ExitOnError)
	jsonOutput := lintCmd.Bool("json", false, "Output issues in JSON format")

	fmtCmd := flag.NewFlagSet("fmt", flag.ExitOnError)
	displayOutput := fmtCmd.Bool("d", false, "Display formatted output instead of writing to file")
	noSort := fmtCmd.Bool("nosort", false, "Do not sort fields within blocks")

	switch os.Args[1] {
	case "lint":
		lintCmd.Parse(os.Args[2:])
		paths := lintCmd.Args()
		if len(paths) == 0 {
			fmt.Fprintln(os.Stderr, "Error: missing file paths for lint command.")
			os.Exit(1)
		}
		if err := lintFiles(paths, *jsonOutput); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	case "fmt":
		fmtCmd.Parse(os.Args[2:])
		paths := fmtCmd.Args()
		if len(paths) == 0 {
			fmt.Fprintln(os.Stderr, "Error: missing file paths for fmt command.")
			os.Exit(1)
		}
		if err := formatFiles(paths, *displayOutput, *noSort); err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			os.Exit(1)
		}
	case "info":
		fmt.Printf("wanflint version: %s\n", getWanfVersion())
		fmt.Printf("Go version: %s\n", runtime.Version())
		fmt.Printf("OS/Arch: %s/%s\n", runtime.GOOS, runtime.GOARCH)
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %q\n", os.Args[1])
		fmt.Fprint(os.Stderr, usage)
		os.Exit(1)
	}
}

func lintFiles(paths []string, jsonOutput bool) error {
	var allErrors []wanf.LintError
	hasParseErrors := false

	for _, path := range paths {
		data, err := os.ReadFile(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", path, err)
			hasParseErrors = true
			continue
		}
		_, errs := wanf.Lint(data)
		if len(errs) > 0 {
			allErrors = append(allErrors, errs...)
		}
	}

	if jsonOutput {
		err := json.MarshalWrite(os.Stdout, allErrors, jsontext.Multiline(true), jsontext.WithIndent("  "))
		if err != nil {
			return fmt.Errorf("could not marshal json: %w", err)
		}
		return nil
	}

	if len(allErrors) > 0 {
		fmt.Fprintln(os.Stderr, "Linter found issues:")
		for _, e := range allErrors {
			fmt.Fprintf(os.Stderr, "  - [%s] %s:%d:%d: %s\n", e.Level, "file", e.Line, e.Column, e.Message)
		}
		return fmt.Errorf("linting found issues")
	}

	if hasParseErrors {
		return fmt.Errorf("errors encountered during linting")
	}

	return nil
}

func formatFiles(paths []string, displayOnly bool, noSort bool) error {
	var wg sync.WaitGroup
	pathsChan := make(chan string, len(paths))
	errChan := make(chan error, len(paths))
	numWorkers := runtime.NumCPU()

	for i := 0; i < numWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for path := range pathsChan {
				err := formatFile(path, displayOnly, noSort)
				if err != nil {
					errChan <- err
				}
			}
		}()
	}

	for _, path := range paths {
		pathsChan <- path
	}
	close(pathsChan)

	wg.Wait()
	close(errChan)

	var allErrors []error
	for err := range errChan {
		allErrors = append(allErrors, err)
	}

	if len(allErrors) > 0 {
		return errors.Join(allErrors...)
	}

	return nil
}

func formatFile(path string, displayOnly bool, noSort bool) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("could not read file %s: %w", path, err)
	}

	// Lint first to catch parsing errors and get the AST
	program, errs := wanf.Lint(data)
	if len(errs) > 0 {
		// In format mode, we still format even if there are non-fatal errors,
		// but we print the errors to stderr.
		// Note: In concurrent mode, these prints might be interleaved, which is acceptable.
		fmt.Fprintf(os.Stderr, "Warning: found %d issues in %s:\n", len(errs), path)
		for _, e := range errs {
			fmt.Fprintf(os.Stderr, "  - %s\n", e.Error())
		}
	}

	// Use the default, opinionated style for the formatter.
	opts := wanf.FormatOptions{Style: wanf.StyleBlockSorted, EmptyLines: true, NoSort: noSort}
	formatted := wanf.Format(program, opts)

	if displayOnly {
		os.Stdout.Write(formatted)
		return nil
	}

	if !bytes.Equal(data, formatted) {
		if err := os.WriteFile(path, formatted, 0644); err != nil {
			return fmt.Errorf("failed to write formatted file %s: %w", path, err)
		}
		fmt.Printf("Formatted %s\n", path)
	}
	return nil
}

func getWanfVersion() string {
	if modInfo, ok := debug.ReadBuildInfo(); ok {
		for _, dep := range modInfo.Deps {
			if dep.Path == "github.com/WJQSERVER/wanf" {
				return dep.Version
			}
		}
		// If it's the main module (e.g. during development/build)
		if modInfo.Main.Path == "github.com/WJQSERVER/wanf" {
			return modInfo.Main.Version
		}
	}
	return "unknown"
}

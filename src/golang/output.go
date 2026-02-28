package main

import (
	"fmt"
	"io"
	"os"
)

func outWriter(s *GameState) io.Writer {
	if s != nil && s.Out != nil {
		return s.Out
	}
	return os.Stdout
}

func outPrint(s *GameState, a ...any) {
	_, _ = fmt.Fprint(outWriter(s), a...)
}

func outPrintln(s *GameState, a ...any) {
	_, _ = fmt.Fprintln(outWriter(s), a...)
}

func outPrintf(s *GameState, format string, a ...any) {
	_, _ = fmt.Fprintf(outWriter(s), format, a...)
}

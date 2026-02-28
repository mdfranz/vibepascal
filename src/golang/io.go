package main

import (
	"bufio"
	"io"
	"os"
	"strings"
	"unicode/utf8"

	"golang.org/x/term"
)

// headlessReader is created once so buffered data isn't lost between calls.
var headlessReader *bufio.Reader

func wrapWriteLn(s *GameState, text string) {
	const maxWidth = 79
	for utf8.RuneCountInString(text) > maxWidth {
		// Find last space at or before maxWidth
		runes := []rune(text)
		spacePos := maxWidth
		for spacePos > 0 && runes[spacePos] != ' ' {
			spacePos--
		}
		if spacePos == 0 {
			spacePos = maxWidth
		}
		outPrintln(s, string(runes[:spacePos]))
		text = strings.TrimLeft(string(runes[spacePos:]), " ")
	}
	outPrintln(s, text)
}

func isDark(s *GameState) bool {
	return s.Turns >= DarkTurn && !s.IsLampLit && s.TempLightTurns == 0
}

func findItemInLoc(name string, loc int, s *GameState) int {
	upper := strings.ToUpper(name)
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == loc && s.Items[i].Name == upper {
			return i
		}
	}
	return 0
}

func look(s *GameState) {
	outPrintln(s)
	if isDark(s) {
		wrapWriteLn(s, "🌑 It is pitch black. You can't see anything.")
		return
	}

	if s.RoomBurning[s.CurrentRoom.ID] > 0 {
		outPrintln(s, "🔥 The room is lit by a growing fire.")
	}

	if s.Turns >= DarkTurn {
		outPrintln(s, "🌕 [The moon hangs in the black sky]")
	} else if s.Turns >= TwilightTurn {
		outPrintln(s, "🌇 [The sky is purple as the sun sets]")
	}

	if s.IsRiding {
		outPrint(s, "🏇 ")
	}
	outPrintf(s, "📍 === %s === ", s.CurrentRoom.Name)
	switch s.CurrentRoom.ID {
	case 1:
		outPrint(s, "🏘️")
	case 2:
		outPrint(s, "📟")
	case 3:
		outPrint(s, "🐴")
	case 4:
		outPrint(s, "⚖️")
	case 5:
		outPrint(s, "🛒")
	case 6:
		outPrint(s, "🌵")
	case 7:
		outPrint(s, "👮")
	case 8, 9, 10, 11, 12, 13:
		outPrint(s, "🏜️")
	}
	outPrintln(s)
	wrapWriteLn(s, s.CurrentRoom.Description)

	if findItemInLoc("MAP", InvLocation, s) > 0 {
		exits := ""
		if s.CurrentRoom.North != nil {
			exits += "NORTH, "
		}
		if s.CurrentRoom.South != nil {
			exits += "SOUTH, "
		}
		if s.CurrentRoom.East != nil {
			exits += "EAST, "
		}
		if s.CurrentRoom.West != nil {
			exits += "WEST, "
		}
		if exits != "" {
			exits = exits[:len(exits)-2]
			outPrintf(s, "Exits: [%s]\n", exits)
		}
	}

	if s.SnakeRoom == s.CurrentRoom.ID {
		outPrintln(s)
		outPrintln(s, "!!! A RATTLESNAKE is coiled here, buzzing its tail angrily !!!")
		outPrintln(s, "One wrong move could be your last.")
	}

	if s.OutlawRoom == s.CurrentRoom.ID {
		outPrintln(s)
		outPrintln(s, "!!! A DIRTY OUTLAW is leaning against the wall, hand on his holster !!!")
		outPrintln(s, `"You don't belong here, stranger," he sneers.`)
	}

	foundItems := false
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == s.CurrentRoom.ID {
			if !foundItems {
				outPrintln(s)
				outPrintln(s, "📦 You see the following here:")
				foundItems = true
			}
			outPrintf(s, "  - %s\n", s.Items[i].Description)
		}
	}
	outPrintln(s)
}

func customReadLn(s *GameState, prompt string) string {
	if s.IsHeadless {
		if headlessReader == nil {
			headlessReader = bufio.NewReader(os.Stdin)
		}
		outPrint(s, prompt)
		line, err := headlessReader.ReadString('\n')
		line = strings.TrimRight(line, "\r\n")
		if err == io.EOF {
			return "QUIT"
		}
		return line
	}

	outPrint(s, prompt)

	fd := int(os.Stdin.Fd())
	oldState, err := term.MakeRaw(fd)
	if err != nil {
		// Fallback to simple readline
		reader := bufio.NewReader(os.Stdin)
		line, _ := reader.ReadString('\n')
		line = strings.TrimRight(line, "\r\n")
		return line
	}

	var lineRunes []rune
	histIdx := s.HistoryCount

	for {
		buf := make([]byte, 4)
		n, err := os.Stdin.Read(buf)
		if err != nil || n == 0 {
			term.Restore(fd, oldState)
			outPrint(s, "\r\n")
			return string(lineRunes)
		}
		b := buf[0]

		switch {
		case b == '\r' || b == '\n':
			term.Restore(fd, oldState)
			outPrint(s, "\r\n")
			line := string(lineRunes)
			if line != "" {
				prev := ""
				if s.HistoryCount > 0 {
					prev = s.History[(s.HistoryCount-1)%MaxHistory]
				}
				if line != prev {
					s.History[s.HistoryCount%MaxHistory] = line
					s.HistoryCount++
				}
			}
			return line

		case b == '\x04': // Ctrl-D
			term.Restore(fd, oldState)
			outPrint(s, "QUIT\r\n")
			return "QUIT"

		case b == '\x7f' || b == '\x08': // Backspace / DEL
			if len(lineRunes) > 0 {
				lineRunes = lineRunes[:len(lineRunes)-1]
				outPrint(s, "\b \b")
			}

		case b == '\x1b': // ESC — read 2 more bytes for arrow keys
			buf2 := make([]byte, 2)
			n2, _ := os.Stdin.Read(buf2)
			if n2 == 2 && buf2[0] == '[' {
				switch buf2[1] {
				case 'A': // Up arrow
					if histIdx > 0 {
						// Erase current line
						for range lineRunes {
							outPrint(s, "\b \b")
						}
						histIdx--
						lineRunes = []rune(s.History[histIdx%MaxHistory])
						outPrint(s, string(lineRunes))
					}
				case 'B': // Down arrow
					if histIdx < s.HistoryCount {
						// Erase current line
						for range lineRunes {
							outPrint(s, "\b \b")
						}
						histIdx++
						if histIdx < s.HistoryCount {
							lineRunes = []rune(s.History[histIdx%MaxHistory])
						} else {
							lineRunes = nil
						}
						outPrint(s, string(lineRunes))
					}
				}
			}

		default:
			if b >= ' ' {
				r, _ := utf8.DecodeRune(buf[:n])
				if r != utf8.RuneError {
					lineRunes = append(lineRunes, r)
					outPrint(s, string(r))
				}
			}
		}
	}
}

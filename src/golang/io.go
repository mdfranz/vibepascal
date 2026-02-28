package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode/utf8"

	"golang.org/x/term"
)

// headlessReader is created once so buffered data isn't lost between calls.
var headlessReader *bufio.Reader

func wrapWriteLn(s string) {
	const maxWidth = 79
	for utf8.RuneCountInString(s) > maxWidth {
		// Find last space at or before maxWidth
		runes := []rune(s)
		spacePos := maxWidth
		for spacePos > 0 && runes[spacePos] != ' ' {
			spacePos--
		}
		if spacePos == 0 {
			spacePos = maxWidth
		}
		fmt.Println(string(runes[:spacePos]))
		s = strings.TrimLeft(string(runes[spacePos:]), " ")
	}
	fmt.Println(s)
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
	fmt.Println()
	if isDark(s) {
		wrapWriteLn("🌑 It is pitch black. You can't see anything.")
		return
	}

	if s.RoomBurning[s.CurrentRoom.ID] > 0 {
		fmt.Println("🔥 The room is lit by a growing fire.")
	}

	if s.Turns >= DarkTurn {
		fmt.Println("🌕 [The moon hangs in the black sky]")
	} else if s.Turns >= TwilightTurn {
		fmt.Println("🌇 [The sky is purple as the sun sets]")
	}

	if s.IsRiding {
		fmt.Print("🏇 ")
	}
	fmt.Printf("📍 === %s === ", s.CurrentRoom.Name)
	switch s.CurrentRoom.ID {
	case 1:
		fmt.Print("🏘️")
	case 2:
		fmt.Print("📟")
	case 3:
		fmt.Print("🐴")
	case 4:
		fmt.Print("⚖️")
	case 5:
		fmt.Print("🛒")
	case 6:
		fmt.Print("🌵")
	case 7:
		fmt.Print("👮")
	case 8, 9, 10, 11, 12, 13:
		fmt.Print("🏜️")
	}
	fmt.Println()
	wrapWriteLn(s.CurrentRoom.Description)

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
			fmt.Printf("Exits: [%s]\n", exits)
		}
	}

	if s.SnakeRoom == s.CurrentRoom.ID {
		fmt.Println()
		fmt.Println("!!! A RATTLESNAKE is coiled here, buzzing its tail angrily !!!")
		fmt.Println("One wrong move could be your last.")
	}

	if s.OutlawRoom == s.CurrentRoom.ID {
		fmt.Println()
		fmt.Println("!!! A DIRTY OUTLAW is leaning against the wall, hand on his holster !!!")
		fmt.Println(`"You don't belong here, stranger," he sneers.`)
	}

	foundItems := false
	for i := 1; i <= MaxItems; i++ {
		if s.Items[i].Location == s.CurrentRoom.ID {
			if !foundItems {
				fmt.Println()
				fmt.Println("📦 You see the following here:")
				foundItems = true
			}
			fmt.Printf("  - %s\n", s.Items[i].Description)
		}
	}
	fmt.Println()
}

func customReadLn(s *GameState, prompt string) string {
	if s.IsHeadless {
		if headlessReader == nil {
			headlessReader = bufio.NewReader(os.Stdin)
		}
		fmt.Print(prompt)
		line, err := headlessReader.ReadString('\n')
		line = strings.TrimRight(line, "\r\n")
		if err == io.EOF {
			return "QUIT"
		}
		return line
	}

	fmt.Print(prompt)

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
			fmt.Print("\r\n")
			return string(lineRunes)
		}
		b := buf[0]

		switch {
		case b == '\r' || b == '\n':
			term.Restore(fd, oldState)
			fmt.Print("\r\n")
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
			fmt.Print("QUIT\r\n")
			return "QUIT"

		case b == '\x7f' || b == '\x08': // Backspace / DEL
			if len(lineRunes) > 0 {
				lineRunes = lineRunes[:len(lineRunes)-1]
				fmt.Print("\b \b")
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
							fmt.Print("\b \b")
						}
						histIdx--
						lineRunes = []rune(s.History[histIdx%MaxHistory])
						fmt.Print(string(lineRunes))
					}
				case 'B': // Down arrow
					if histIdx < s.HistoryCount {
						// Erase current line
						for range lineRunes {
							fmt.Print("\b \b")
						}
						histIdx++
						if histIdx < s.HistoryCount {
							lineRunes = []rune(s.History[histIdx%MaxHistory])
						} else {
							lineRunes = nil
						}
						fmt.Print(string(lineRunes))
					}
				}
			}

		default:
			if b >= ' ' {
				r, _ := utf8.DecodeRune(buf[:n])
				if r != utf8.RuneError {
					lineRunes = append(lineRunes, r)
					fmt.Print(string(r))
				}
			}
		}
	}
}


package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

type Tuple struct {
    a,b int
}

// greatest common divisor (GCD) via Euclidean algorithm
func GCD(a, b int) int {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}
	return a
}

// find Least Common Multiple (LCM) via GCD
func LCM(a, b int, integers ...int) int {
	result := a * b / GCD(a, b)

	for i := 0; i < len(integers); i++ {
		result = LCM(result, integers[i])
	}

	return result
}

type Mod struct {
    time, mul int
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    lineOne := true
    buses := make([]string,0)
    for scanner.Scan() {
        line := scanner.Text()
        if lineOne {
            lineOne = false
        } else {
            buses = strings.Split(line, ",")
        }
    }

    mods := make([]Mod,0)
    for i,v := range buses {
        if v != "x" {
            id,_ := strconv.Atoi(v)
            mods = append(mods, Mod{i,id})
        /*
            if depart % id == 0 {
                fmt.Println(id)
                earliest = depart
                ID = id
            } else {
                mod := depart%id
                mod = (depart-mod)
                cur := (mod+id)
                if cur < earliest {
                    earliest = cur
                    ID = id
                }
            }*/
        }
    }
    fmt.Println(mods)
}

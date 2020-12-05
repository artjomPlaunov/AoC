
package main

import (
    "bufio"
    "fmt"
    "os"
    "regexp"
    "strconv"
)

type passwordConstraint struct {
    letterRange []string
    letter      byte
    password    string
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    rangeRegex := regexp.MustCompile("[0-9]+")
    letterRegex := regexp.MustCompile("[a-z]:")
    passwordRegex := regexp.MustCompile("[a-z][a-z]+")

    passwords := make([]passwordConstraint,0)
    validCount := 0
    validPasswords := make([]passwordConstraint,0)
    for scanner.Scan() {
        line := scanner.Text()
        letterRange := rangeRegex.FindAllString(line,-1)
        letter := letterRegex.FindAllString(line,-1)[0][0]
        password := passwordRegex.FindAllString(line,-1)[0]
        cur := passwordConstraint{letterRange, letter, password}
        passwords = append(passwords, cur)
        if isValidPassword(cur) {
            validCount += 1
            validPasswords = append(validPasswords, cur)
        }
    }
    fmt.Println(validCount)
}

func isValidPassword(cur passwordConstraint) bool {

    lo, _ := strconv.Atoi(cur.letterRange[0])
    hi, _ := strconv.Atoi(cur.letterRange[1])

    c1 := cur.password[lo-1]
    c2 := cur.password[hi-1]
    l := cur.letter

    return (l == c1) != (l == c2)
}

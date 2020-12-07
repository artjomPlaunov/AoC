package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
)

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    Q := make([][]string,0)
    cur := make([]string, 0)
    for scanner.Scan() {
        line := scanner.Text()
        if len(line) == 0 {
            Q = append(Q, cur)
            cur = make([]string,0)
        } else {
            cur = append(cur, line)
        }
    }
    Q = append(Q, cur)

    res := 0
    for _, group := range Q {
        for c := 'a'; c <= 'z'; c++ {
            if yesGroup(group,byte(c)) {
                res += 1
            }
        }
    }
    fmt.Println(res)
}

func yesGroup(g []string, ch byte) bool {
    for _, line := range g {
        flag := false
        for _, c := range line {
            if byte(c) == ch {
                flag = true
            }
        }
        if !flag {
            return false
        }
    }
    return true
}

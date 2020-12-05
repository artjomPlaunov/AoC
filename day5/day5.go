
package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "sort"
    //"regexp"
)

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    max := 0.0
    lst := make([]float64,0)
    for scanner.Scan() {
        line := scanner.Text()
        row,column := decode(line)
        id := row * 8 + column
        lst = append(lst, id)
        if id > max {
            max = id
        }
    }
    sort.Float64s(lst)
    for i := 0; i < len(lst)-1; i++ {
        if lst[i+1]-lst[i] > 1 {
            fmt.Println(lst[i],lst[i+1])
        }
    }
    fmt.Println(lst)
}

func decode(s string) (float64, float64) {
    pow := 6.0
    row := 0.0
    for _, c := range s[:7] {
        if c == 'B' {
            row += math.Pow(2,pow)
        }
        pow -= 1
    }

    pow = 2.0
    col := 0.0
    for _, c := range s[7:] {
        if c == 'R' {
            col += math.Pow(2,pow)
        }
        pow -= 1
    }
    return row,col
}

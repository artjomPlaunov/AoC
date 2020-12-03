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
    graph := make([][]byte, 0)
    for scanner.Scan() {
        line := scanner.Text()
        graph = append(graph, []byte(line))
    }
    s := func(x, y int) int {
        res, m, n := 0,0,0
        for m < len(graph) {
            if graph[m][n] == '#' {
                res += 1
            }
            m += y
            n = (n+x)%len(graph[0])
        }
        return res
    }

    fmt.Println(s(1,1)*s(3,1)*s(5,1)*s(7,1)*s(1,2))
}

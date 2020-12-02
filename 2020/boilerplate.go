
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
    for scanner.Scan() {
        line := scanner.Text()
    }

}

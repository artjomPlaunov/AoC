
package main

import (
    "bufio"
    "fmt"
    "os"
    "regexp"
    "strconv"
    "strings"
)

type Rule struct {
    Num     int
    Name    string
}



func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    graph := make(map[string][]Rule)
    for scanner.Scan() {
        line := scanner.Text()

        name := getName(line)
        rules := getRules(line)
        graph[name] = rules
    }

    visited     := make(map[string]bool)
    contains    := make(map[string]bool)
    res := 0

    for k,_ := range graph {
        if k == "shiny gold" {
            continue
        }
        if hasPath(k, "shiny gold", graph, visited, contains) {
            res += 1
        }
    }

    numBags := make(map[string]int)
    countBags("shiny gold", graph, numBags)
    fmt.Println(numBags["shiny gold"])
}

func countBags( origin string,
                graph map[string][]Rule,
                numBags map[string]int) int {
    fmt.Println(numBags)
    if _,ok := numBags[origin]; ok {
        return numBags[origin]
    } else if len(graph[origin]) == 0 {
        numBags[origin] = 0
        return 0
    }
    res := 0
    for _,rule := range graph[origin] {
        num := countBags(rule.Name, graph, numBags)
        if num == 0 {
            res += rule.Num
        } else {
            res += (rule.Num*num + rule.Num)
        }
    }
    numBags[origin] = res
    return res
}

func hasPath(origin, dest string, graph map[string][]Rule,
                visited, contains map[string]bool) bool {
    if origin == dest {
        return true
    } else if visited[origin] {
        return contains[origin]
    } else {
        visited[origin] = true
        for _, rule := range graph[origin] {
            if hasPath(rule.Name, "shiny gold", graph, visited, contains) {
                contains[origin] = true
                return true
            }
        }
        contains[origin] = false
        return false
    }
}

/* Parsing functions */

func getRules(line string) []Rule {
    res := make([]Rule,0)
    r, _ := regexp.Compile("[0-9]+ [a-z]+ [a-z]+")
    for _, rule := range r.FindAllString(line, -1) {
        fields := strings.Fields(rule)
        num,_ := strconv.Atoi(fields[0])
        name := fields[1] + " " + fields[2]
        res = append(res, Rule{num,name})
    }
    return res
}

func getName(line string) string {
    words := strings.Fields(line)
    return words[0] + " " + words[1]
}

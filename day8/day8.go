
package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
    "strconv"
    "strings"
)

type Op struct {
    Op      string
    Val     int
}

type Process struct {
    Acc, Pc         int
    Instructions    []Op
}

func (p *Process) Init(f string) {
    file, _ := os.Open(f)
    defer file.Close()

    scanner := bufio.NewScanner(file)
    instructions := make([]Op, 0)
    for scanner.Scan() {
        line := scanner.Text()
        words := strings.Fields(line)
        offset,_ := strconv.Atoi(words[1])
        instructions = append(instructions, Op{words[0], offset})
    }
    p.Instructions = instructions
    p.Acc = 0
    p.Pc = 0
}

func (p *Process) IsInfinite() bool {
    // Checks if instruction at PC was executed already
    visited := make(map[int]bool)

    for p.Pc != len(p.Instructions) {
        if _,ok := visited[p.Pc]; ok {
            return true
        }
        visited[p.Pc] = true
        p.Execute()
    }
    // Reset Process
    p.Reset()
    return false
}

func (p *Process) Run() {
    for p.Pc != len(p.Instructions) {
        p.Execute()
    }
}

func (p *Process) Reset() {
    p.Pc = 0
    p.Acc = 0
}

func (p *Process) Execute() {
    i := p.Instructions[p.Pc]
    switch i.Op {
        case "acc":
            p.Acc += i.Val
            p.Pc += 1
        case "jmp":
            p.Pc += i.Val
        case "nop":
            p.Pc += 1
    }
}

func main() {
    p := &Process{}
    p.Init(os.Args[1])

    for i,_ := range p.Instructions {
        if p.Instructions[i].Op == "nop" {
            p.Instructions[i].Op = "jmp"
            if p.IsInfinite() {
                p.Instructions[i].Op = "nop"
            } else {
                p.Run()
                fmt.Println(p.Acc)
                return
            }
        } else if p.Instructions[i].Op == "jmp" {
            p.Instructions[i].Op = "nop"
            if p.IsInfinite() {
                p.Instructions[i].Op = "jmp"
            } else {
                p.Run()
                fmt.Println(p.Acc)
                return
            }
        }
    }
}

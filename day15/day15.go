
package main

import (
    "fmt"
)

func main() {

    mem := make(map[int][]int)
    mem[0] = []int{1}
    mem[1] = []int{2}
    mem[5] = []int{3}
    mem[10] = []int{4}
    mem[3] = []int{5}
    mem[12] = []int{6}
    mem[19] = []int{7}
    turn := 8
    last := 19

    for turn <= 30000000 {
        // If number is first spoken
        if len(mem[last]) == 1 {
            last = 0
            mem[0] = append(mem[0], turn)
        } else {
            l := len(mem[last])-1
            last = mem[last][l] - mem[last][l-1]
            if _,ok := mem[last]; ok {
                mem[last] = append(mem[last], turn)
            } else {
                mem[last] = []int{turn}
            }
        }
        turn += 1
    }
    fmt.Println(last)
}

package main

import (
    "bufio"
    "fmt"
    "os"
    "sort"
    "strconv"
)

func twoSumSorted(input []int, i, target int) (bool, int, int) {
    j := len(input)-1
    for i < j {
        if input[i] + input[j] == target {
            return true, input[i], input[j]
        } else if input[i] + input[j] < target {
            i += 1
        } else {
            j -= 1
        }
    }
    return false, 0, 0
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    var input[]int
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        val, _ := strconv.Atoi(scanner.Text())
        input = append(input, val)
    }

    /* Part 2 - ThreeSum - O(n^2)*/

    // O(nlogn) sort on input
    sort.Ints(input)
    for i := 0; i < len(input)-2; i++ {
        if ok,num1,num2 := twoSumSorted(input, i+1, 2020-input[i]); ok {
            fmt.Println(num1 * num2 * input[i])
            break
        }
    }


    /* Part 1 - TwoSum - O(n)
    var res int
    hash := make(map[int]int)
    for i, v := range input {
        if _, ok := hash[2020-v]; ok {
            res = v
            break
        } else {
            hash[v] = i
        }
    }
    fmt.Println(res * (2020-res)) 
    */
}

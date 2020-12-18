
package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
    "strconv"
)

func main() {
    file, _ := os.Open(os.Args[1])
    defer file.Close()

    scanner := bufio.NewScanner(file)
    xmas := make([]int,0)
    for scanner.Scan() {
        line := scanner.Text()
        val,_ := strconv.Atoi(line)
        xmas = append(xmas, val)
    }
    l := 25
    k := -1
    for i := l+1; i < len(xmas); i++ {
        k = xmas[i]
        if !twoSum(xmas, k, i-l, i-1) {
            break
        }
    }

    sum, hash := 0, make(map[int]int)
    lo,hi := -1,-1

    for j, v := range xmas {
        sum += v
        if i, ok := hash[sum-k]; ok {
            if (j-i) >= 2 {
                lo,hi = i+1,j
                break
            }
        }
        hash[sum] = j
    }
    min,max := xmas[lo],xmas[lo]
    for i := lo+1; i <= hi; i++ {
        if xmas[i] < min {
            min = xmas[i]
        }
        if xmas[i] > max {
            max = xmas[i]
        }
    }
    res := xmas[lo:hi+1]
    fmt.Println(res)
    a := 0
    for _,v := range res {
        a += v
    }
    fmt.Println(a,k,min,max)
    fmt.Println(max+min)
}

func twoSum(xmas []int, sigma, lo, hi int) bool {
    hash := make(map[int]int)
    for i := lo; i <= hi; i++ {
        cur := xmas[i]
        if _,ok := hash[sigma-cur]; ok {
            return true
        }
        hash[cur] = i
    }
    return false
}

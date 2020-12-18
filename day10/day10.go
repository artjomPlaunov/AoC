
package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
    "strconv"
    "sort"
)

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    res := make([]int, 0)
    res = append(res,0)
    for scanner.Scan() {
        line := scanner.Text()
        num,_ := strconv.Atoi(line)
        res = append(res,num)
    }
    sort.Ints(res)
    res = append(res, res[len(res)-1]+3)
    ones,threes := 0,0
    for i := 0; i < len(res)-1; i++ {
        if res[i+1]-res[i] == 3 {
            threes += 1
        } else {
            ones += 1
        }
    }
    dp := make([]int, len(res))
    dp[0] = 1
    dp[1] = 1
    if res[2] - res[1] <= 3 {
        dp[2] = dp[0] + dp[1]
    } else {
        dp[2] = dp[1]
    }
    for i := 3; i < len(res); i++ {
        if res[i] - res[i-3] <= 3 {
            dp[i] = dp[i-1] + dp[i-2] + dp[i-3]
        } else if res[i] - res[i-2] <= 3 {
            dp[i] = dp[i-1] + dp[i-2]
        } else {
            dp[i] = dp[i-1]
        }
    }
    fmt.Println(dp[len(dp)-1])
}
/* Initial solution (too slow on large inputs)
func perm(nums []int, memo map[string]bool){
    s := intSliceToString(nums)
    if _,ok := memo[s]; ok {
        return
    } else if len(nums) == 2 {
        memo[s] = true
        return
    } else {
        flag := true
        for flag {
            for i := 0; i < len(nums)-2; i++ {
                if nums[i+2]-nums[i] <= 3 {
                    cpy := make([]int, len(nums))
                    copy(cpy,nums)
                    cpy = append(cpy[:i+1],cpy[i+2:]...)
                    perm(cpy,memo)
                    continue
                }
            flag = false
            }
        }
        s := intSliceToString(nums)
        memo[s] = true
    }
}

func intSliceToString(nums []int) string {
    resSlice := []string{}
    for _, v := range nums {
        test := strconv.Itoa(v)
        resSlice = append(resSlice, test)
    }
    res := strings.Join(resSlice, "+")
    return res
}
*/








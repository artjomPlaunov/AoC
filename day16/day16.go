
package main

import (
    "bufio"
    "fmt"
    "os"
    "regexp"
    "strings"
    "strconv"
)

type Cat struct {
    valid   bool
    has     map[string]bool
}

func contains(t [][]int, index int, isValid []Cat, key string) bool {
    for i := 0; i < len(t); i++ {
        if _,ok := isValid[t[i][index]].has[key]; !ok {
            return false
        }
    }
    fmt.Println(index,key)
    return true
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    isValid := make([]Cat,1000)
    for i := 0; i < 1000; i++ {
        isValid[i] = Cat{false, make(map[string]bool)}
    }
    nearby := make([]string,0)
    constraints := make([]string,0)
    myTicket := make([]int, 0)
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        if len(line) == 0 {
            continue
        } else if line == "your ticket:" {
            scanner.Scan()
            myTix := scanner.Text()
            tmp := strings.Split(myTix,",")
            for _,t := range tmp {
                t2,_ := strconv.Atoi(t)
                myTicket = append(myTicket,t2)
            }
        } else if line == "nearby tickets:" {
            for scanner.Scan() {
                line := scanner.Text()
                nearby = append(nearby, line)
            }
            break
        } else {
            constraints = append(constraints, line)
        }

    }

    for _,constraint := range constraints {
        r,_ := regexp.Compile("[0-9]+-[0-9]+")
        r2,_ := regexp.Compile("[a-z :]+")
        category := r2.FindAllString(constraint,-1)
        var cat string
        if len(category) > 0 {
            cat = category[0]
        }
        fields := r.FindAllString(constraint,-1)
        for _,field := range fields {
            c := strings.Split(field,"-")
            lo,_ := strconv.Atoi(c[0])
            hi,_ := strconv.Atoi(c[1])
            for i := lo; i <= hi; i++ {
                isValid[i].valid = true
                isValid[i].has[string(cat)] = true
            }
        }
    }
    nearbyTickets := make([]string,0)
    for _,n := range nearby {
        flag := true
        vals := strings.Split(n,",")
        for _,val := range vals {
            v,_ := strconv.Atoi(val)
            if isValid[v].valid == false {
                flag = false
            }
        }
        if flag {
            nearbyTickets = append(nearbyTickets,n)
        }
    }

    tix := make([][]int,0)
    for _,v := range nearbyTickets {
        res := make([]int,0)
        s := strings.Split(v,",")
        for _,val := range s {
            num,_ := strconv.Atoi(val)
            res = append(res,num)
        }
        tix = append(tix,res)
    }
    res := 1
    count := 0
    for i := 0; i < len(tix[0]); i++ {
        if      (contains(tix, i, isValid, "departure location: ")  ||
                contains(tix, i, isValid, "departure station: ")  ||
                contains(tix, i, isValid, "departure platform: ")  ||
                contains(tix, i, isValid, "departure track: ")  ||
                contains(tix, i, isValid, "departure time: ")  ||
                contains(tix, i, isValid, "departure date: ")){
            res *= myTicket[i]
            count += 1
        }
    }
    fmt.Println(res,count)
}


package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
)

type Coord struct {
    X,Y int
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    seats := make([][]rune,0)
    for scanner.Scan() {
        line := scanner.Text()
        seats = append(seats, []rune(line))
    }

    coords := []Coord{{-1,-1}, {-1,0}, {-1,1}, {0,1}, {1,1},
                        {1,0}, {1,-1}, {0,-1}}

    isValidCoord := func(coord Coord) bool{
        x,y := coord.X,coord.Y
        if x < 0 || x >= len(seats) || y < 0 || y >= len(seats[x]) {
            return false
        }
        return true
    }

    countTaken := func(i,j int) int {
        res := 0
        for _, c := range coords {
            foundTaken := false
            cur := Coord{i+c.X,j+c.Y}
            for isValidCoord(cur) {
                v := seats[cur.X][cur.Y]
                if v == '#' {
                    foundTaken = true
                    break
                } else if v == 'L' {
                    break
                } else {
                    cur.X += c.X
                    cur.Y += c.Y
                }
            }
            if foundTaken {
                res += 1
            }
        }
        return res
    }

    stable := false
    tmp := make([][]rune, len(seats))
    for !stable {
        tmp = make([][]rune, len(seats))
        for i,_ := range seats {
            tmp[i] = make([]rune, len(seats[i]))
        }
        stable = true
        for i, _ := range seats {
            for j,v := range seats[i] {
                if v == 'L' {
                    taken := countTaken(i,j)
                    if taken == 0 {
                        tmp[i][j] = '#'
                        stable = false
                    } else {
                        tmp[i][j] = v
                    }
                } else if v == '#' {
                    taken := countTaken(i,j)
                    if taken >= 5 {
                        tmp[i][j] = 'L'
                        stable = false
                    } else {
                        tmp[i][j] = v
                    }
                } else {
                    tmp[i][j] = v
                }
            }
        }
        seats = make([][]rune, len(tmp))
        for i,v := range tmp {
            seats[i] = make([]rune,len(v))
            copy(seats[i],v)
        }
    }
    for _,v := range seats {
        fmt.Println(string(v))
    }

    taken := 0
    for _,v := range seats {
        for _,c := range v {
            if c == '#' {
                taken += 1
            }
        }
    }
    fmt.Println(taken)
}




package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
    "strconv"
)

type Coord struct {
    X,Y int
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)

    dHash := map[rune]int{
        'N': 0,
        'E': 1,
        'S': 2,
        'W': 3,
    }
    d := map[int]Coord{
        0: {0,1},
        1: {1,0},
        2: {0,-1},
        3: {-1,0},
    }

    cur := Coord{0,0}
    wayPoint := Coord{10,1}

    updateCoord := func(c Coord, dir, dist int) Coord {
        offset := d[dir]
        x,y := dist*offset.X, dist*offset.Y
        return Coord{c.X+x,c.Y+y}
    }

    for scanner.Scan() {
        line := scanner.Text()
        ins := rune(line[0])
        if ins == 'L' || ins == 'R' {
            degrees,_ := strconv.Atoi(line[1:])
            if ins == 'L' {
                degrees *= -1
            }
            x,y := 0,0
            fmt.Println(degrees)
            if degrees == 90 || degrees == -270 {
                if wayPoint.X > 0 {
                    y = -wayPoint.X
                } else if wayPoint.X < 0{
                    y = -wayPoint.X
                } else {
                    y = 0
                }
                if wayPoint.Y > 0 {
                    x = wayPoint.Y
                } else if wayPoint.Y < 0 {
                    x = wayPoint.Y
                } else {
                    x = 0
                }
            } else if degrees == 180 || degrees == -180 {
                x = -wayPoint.X
                y = -wayPoint.Y
            } else if degrees == 270 || degrees == -90 {
                if wayPoint.X > 0 {
                    y = wayPoint.X
                } else if wayPoint.X < 0{
                    y = wayPoint.X
                } else {
                    y = 0
                }
                if wayPoint.Y > 0 {
                    x = -wayPoint.Y
                } else if wayPoint.Y < 0{
                    x = -wayPoint.Y
                } else {
                    x = 0
                }
            }
            wayPoint = Coord{x,y}
        } else {
            dist,_ := strconv.Atoi(line[1:])
            if ins == 'F' {
                cur.X += (dist*wayPoint.X)
                cur.Y += (dist*wayPoint.Y)
            } else {
                wayPoint = updateCoord(wayPoint, dHash[ins],dist)
            }
        }
        fmt.Println(cur,wayPoint)
    }
    fmt.Println(cur)

}

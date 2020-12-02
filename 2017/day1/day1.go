package main

import (
  "fmt"
  "strconv"
  "strings"
)

type Coord struct {
  x, y int
}

/* Custom mod function so that (-3 mod 4) returns 3 instead of -1 */
func mod(x, y int) int {
  res := x % y
  if res >= 0 {
    return res
  } else {
    return res + y
  }
}

func update_coord(facing, steps int, coord []int) []int {
  if facing == 0 {
    coord[0] += steps
  } else if facing == 1 {
    coord[1] += steps
  } else if facing == 2 {
    coord[0] -= steps
  } else {
    coord[1] -= steps
  }
  return coord
}

func update_visited(facing, steps int, coord []int , visited map[Coord]bool) {
  sign := 1
  if facing == 2 || facing == 3 {
    sign = -1
  }

  if facing == 0 || facing == 2 {
    for i := 1; i <= steps; i += 1 {
      if visited[Coord{sign*i+coord[0],coord[1]}] == true {
        fmt.Println("visited ", Coord{sign*i+coord[0],coord[1]})
      } else {
        visited[Coord{sign*i+coord[0],coord[1]}] = true
      }
    }
  } else {
    for i := 1; i <= steps; i += 1 {
      if visited[Coord{coord[0],sign*i+coord[1]}] == true {
        fmt.Println("visited ", Coord{coord[0],sign*i+coord[1]})
      } else {
        visited[Coord{coord[0],sign*i+coord[1]}] = true
      }
    }
  }
}

func main() {
  input := "L4, R2, R4, L5, L3, L1, R4, R5, R1, R3, L3, L2, L2, R5, R1, L1, L2, R2, R2, L5, R5, R5, L2, R1, R2, L2, L4, L1, R5, R2, R1, R1, L2, L3, R2, L5, L186, L5, L3, R3, L5, R4, R2, L5, R1, R4, L1, L3, R3, R1, L1, R4, R2, L1, L4, R5, L1, R50, L4, R3, R78, R4, R2, L4, R3, L4, R4, L1, R5, L4, R1, L2, R3, L2, R5, R5, L4, L1, L2, R185, L5, R2, R1, L3, R4, L5, R2, R4, L3, R4, L2, L5, R1, R2, L2, L1, L2, R2, L2, R1, L5, L3, L4, L3, L4, L2, L5, L5, R2, L3, L4, R4, R4, R5, L4, L2, R4, L5, R3, R1, L1, R3, L2, R2, R1, R5, L4, R5, L3, R2, R3, R1, R4, L4, R1, R3, L5, L1, L3, R2, R1, R4, L4, R3, L3, R3, R2, L3, L3, R4, L2, R4, L3, L4, R5, R1, L1, R5, R3, R1, R3, R4, L1, R4, R3, R1, L5, L5, L4, R4, R3, L2, R1, R5, L3, R4, R5, L4, L5, R2"

  input = strings.Replace(input, ",", "", -1)
  parsed_input := strings.Split(input, " ")

  /* 
    0 - North
    1 - East
    2 - South
    3 - West
  */

  facing  := 0
  coord   := []int{0,0}
  visited := make(map[Coord]bool)
  visited[Coord{0,0}] = true

  for _, instruction := range parsed_input {
    if instruction[0] == 'L' {
      facing = mod((facing-1), 4)
    } else {
      facing = mod((facing+1), 4)
    }
    steps, _ := strconv.Atoi(instruction[1:])
    update_visited(facing, steps, coord, visited)
    coord = update_coord(facing, steps, coord)
  }
  fmt.Println(coord)
}

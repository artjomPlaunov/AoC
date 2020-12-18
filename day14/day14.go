
package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "strings"
    "strconv"
)

func updateMask(mask string) map[uint64]uint64 {
    res := make(map[uint64]uint64)
    for i := 35; i >= 0; i-- {
        if mask[i] == '1' {
            res[uint64(35-i)] = 1
        } else if mask[i] == 'X' {
            res[uint64(35-i)] = 0
        }
    }
    return res
}
/*
func applyMask(val uint64, mask map[uint64]uint64) uint64 {
    for k,v := range mask {
        if v == 1 {
            val |= (1<<k)
        } else {
            makeMask := uint64(math.Pow(2,36) - 1)
            makeMask ^= (1<<k)
            val &= makeMask
        }
    }
    return val
}*/

func setBit(a uint64, b BitSet) uint64 {
    if b.on {
        a |= (1<<b.bit)
    } else {
        makeMask := uint64(math.Pow(2,36)-1)
        makeMask ^= (1<<b.bit)
        a &= makeMask
    }
    return a
}

func getAddress(address string) uint64 {
    parse := address[4:]
    parse = parse[:len(parse)-1]
    res,_ := strconv.Atoi(parse)
    return uint64(res)
}

type BitSet struct {
    bit uint64
    on  bool
}
func permute(x []uint64) [][]BitSet {

    if len(x) == 0 {
        return make([][]BitSet,0)
    }
    on  := BitSet{x[0], true}
    off := BitSet{x[0], false}
    rec := permute(x[1:])
    l := len(rec)
    for i := 0; i < l; i++ {
        rec[i] = append(rec[i],on)
        tmp := make([]BitSet, len(rec[i]))
        copy(tmp,rec[i])
        tmp[len(tmp)-1] = off
        rec = append(rec, tmp)
    }
    if l == 0 {
        rec = append(rec, []BitSet{on})
        rec = append(rec, []BitSet{off})
    }
    return rec
}

func applyMask(address uint64, mask map[uint64]uint64) []uint64 {
    x := make([]uint64, 0)
    res := make([]uint64, 0)
    for k,v := range mask {
        if v == 0 {
            x = append(x, k)
        } else {
            address |= (1<<k)
        }
    }
    if len(x) == 0 {
        return append(res,address)
    }
    sets := permute(x)
    for _,set := range sets {
        a := address
        for _,bit := range set {
            a = setBit(a, bit)
        }
        res = append(res, a)
    }
    fmt.Println(res)
    return res
}

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    mask    := make(map[uint64]uint64)
    mem     := make(map[uint64]uint64)

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        fields := strings.Fields(line)
        if fields[0] == "mask" {
            mask = updateMask(fields[2])
        } else {
            baseAddress := getAddress(fields[0])
            v,_ := strconv.Atoi(fields[2])
            val := uint64(v)
            addresses := applyMask(baseAddress, mask)
            for _,address := range addresses {
                mem[address] = val
            }
        }
    }
    sum := uint64(0)
    for _,v := range mem {
        sum += v
    }
    fmt.Println("sum",sum)
}

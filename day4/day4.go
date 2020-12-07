
package main

import (
    "bufio"
    "fmt"
    "os"
    //"regexp"
    "strconv"
    "strings"
)

func main() {
    file, _ := os.Open("./input.txt")
    defer file.Close()

    scanner := bufio.NewScanner(file)
    input := make([]map[string]string,0)
    input = append(input, make(map[string]string))

    for scanner.Scan() {
        line := scanner.Text()
        if len(line) == 0 {
            input = append(input, make(map[string]string))
            continue
        }
        cur := input[len(input)-1]
        fields := strings.Fields(line)
        for _, field := range fields {
            cur[field[:3]] = field[4:]
        }
    }
    fields := []string{"byr","iyr","eyr","hgt","hcl","ecl","pid","cid"}
    valid := make([]map[string]string,0)

    for _, passport := range input {
        flag := true
        for _, field := range fields {
            if field == "cid" {
                continue
            } else {
                if _, ok := passport[field]; !ok {
                    flag = false
                } else {
                    if !isValidField(field, passport[field]) {
                        flag = false
                    }
                }
            }
        }
        if flag == true {
            valid = append(valid, passport)
        }
    }
    fmt.Println(len(input), len(valid))
}

func isValidField(field, value string) bool {

    isValidYear := func(year string, lo,hi int) bool {
        if len(value) != 4 {
            return false
        } else {
            byr,_ := strconv.Atoi(value)
            if byr < lo || byr > hi {
                return false
            } else {
                return true
            }
        }

    }
    switch field {
        case "byr":
            return isValidYear(value, 1920, 2002)
        case "iyr":
            return isValidYear(value, 2010, 2020)
        case "eyr":
            return isValidYear(value, 2020, 2030)
        case "hgt":
            unit := value[len(value)-2:]
            if unit != "cm" && unit != "in" {
                return false
            }
            hgt,_ := strconv.Atoi(value[:len(value)-2])
            if unit == "cm" {
                return hgt >= 150 && hgt <= 193
            } else {
                return hgt >= 59 && hgt <= 76
            }
        case "hcl":
            if len(value) != 7 {
                return false
            } else {
                /*
                if value[0] != '#' {
                    return false
                } else {
                    for i := 1; i < 7; i++ {
                        c := value[i]
                        if c < '0' && c > '9' && c <'a' && c > 'f' {
                            return false
                        }
                    }
                    return true
                }*/
                return true
            }
        case "ecl":
            ecl := map[string]bool{
                "amb":true, "blu":true, "brn":true, "gry":true,
                "grn":true, "hzl":true, "oth":true,
            }
            if _,ok := ecl[value]; !ok {
                return false
            } else {
                return true
            }
        case "pid":
            if len(value) != 9 {
                return false
            } else {
                /*
                for _,c := range value {
                    if c < '0' || c > '9' {
                        return false
                    }
                }*/
                return true
            }
        default:
            return true
    }
}

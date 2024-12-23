<center>

# Garv Nagori

## 2021CS10549
</center>

Four problems have been solved in Prolog

### First Problem
Given two lists, the program finds if there exists a subsequence of the first list in the second list.
To run: open the program using ```swipl subseq.P```.
The query to call the function is 

    subsequence(<Smaller List>, <Gretater List>).

### Second Problem
Given a list, the program finds if there are three elements in a row. It outputs true if there are none.
To run: open the program using ```swipl triplicates.P```.
The query to call the function is

    has_no_triplicates(<list>).

### Third Problem
Given a list, the program finds if there is any combination of the operators ```+```, ```-``` or ```=``` such that the equation is satisfied. Only one = is allowed in the equation. To output more solutions, press ```;``` to continue the query. Once all are exhausted, a ```false``` is printed and the query is completed.
To run: open the program using ```swipl arith.P```.
The query to call the function is 

    arith(<list>).

### Fourth Problem
This program is made to solve the ***River Crossing Problem***. There are four people stuck on the left side - *Alice, Bob, Carol, Davis* and only one boat. There are some extraneous conditions. The program is scalable, with more people and extra conditions can be put without much effort. 
To run: open the program using ```swipl ABCD.P```
The query to call this function is

    abcd.
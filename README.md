# order-filter

## Usage:
```
java -jar orderfilter.jar 2018-01-01 2019-01-01
0-1 months -> 3 orders
1-2 months -> 1 orders
2-3 months -> 1 orders
3-4 months -> 1 orders
> 12 months -> 2 orders
```
or
```
java -jar orderfilter.jar 2018-01-01 2019-01-01 1-3 3-6 >6
1-3 months -> 3 orders
3-6 months -> 2 orders
> 6 months -> 2 orders
```

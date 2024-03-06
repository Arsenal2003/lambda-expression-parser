#!/usr/bin/bash

sum=0

while read -r line; do
	aux=$(echo "$line" | sed 's/points//g')
	num=$(echo "$aux" | cut -c2-)
	sum=$(($sum + $num))
done < <(runhaskell test.hs | grep '+')

echo "Grade: $sum/150"

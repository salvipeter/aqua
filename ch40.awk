BEGIN { RS = " " }
{ h[++n] = $0 }
END {
    for (i = 2; i < n; i++)
        if (h[i-1] < h[i] && h[i] > h[i+1] || h[i-1] > h[i] && h[i] < h[i+1])
            b[++k] = h[i]
    for (i = 1; i <= k; i += 2) {
        drop1 = drop2 = b[i]
        found1 = found2 = 0
        for (j = i - 2; j >= 1; j -= 2) {
            drop1 = min(drop1, b[j+1])
            if (b[j] >= b[i]) {
                found1 = 1
                break
            }
        }
        for (j = i + 2; j <= k; j += 2) {
            drop2 = min(drop2, b[j-1])
            if (b[j] >= b[i]) {
                found2 = 1
                break
            }
        }
        if (found1 && found2)
            sum += min(b[i] - drop1, b[i] - drop2)
        else if (found1)
            sum += b[i] - drop1
        else if (found2)
            sum += b[i] - drop2
        else
            sum += b[i]
    }
    print sum
}

function min(x, y) { return x < y ? x : y }

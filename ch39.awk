BEGIN { RS = " "; player = 1; starter = 1 }
{
    n++
    score[player] += $0
    if (score[player] > 500) {
        sum += $0
        if (player == 1)
            wins++
        score[1] = score[2] = n = 0
        starter = 3 - starter
        player = starter
    } else if (n == 3) {
        player = 3 - player
        n = 0
    }
}
END { print wins * sum }

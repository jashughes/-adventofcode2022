instr <- read.table("02.txt", col.names = c("them", "me"))
key <- setNames(rep(0:2, 2), c("A", "B", "C", "X", "Y", "Z"))
instr$them <- unname(key[instr$them])
instr$me <- unname(key[instr$me])
win_points <- function(them, me) {
  ifelse(them == me, 3, ifelse((them + 1) %% 3 == me, 6, 0))
}
score_round <- function(them, me) { win_points(them, me) + me + 1 }
cheat <- function(them, goal) {
  ifelse(goal == 1, them, ifelse(goal == 2, (them + 1) %% 3, (them + 2) %% 3))
}

message("Part 1: ", sum(score_round(instr$them, instr$me)))
message("Part 2: ", sum(score_round(instr$them, cheat(instr$them, instr$me))))
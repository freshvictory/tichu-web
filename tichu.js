/**
 * @returns {Game}
 */
export function newGame() {
  return {
    ourScore: 0,
    theirScore: 0,
    history: [],
  };
}

/**
 * @param turn {Turn}
 * @param game {Game}
 * @returns {Game}
 */
export function score(turn, game) {
  const { us: ourTakenPoints, them: theirTakenPoints } =
    calculateTakenPoints(turn);

  const { us: ourBetPoints, them: theirBetPoints } = turnBetPoints(turn);

  return {
    ourScore: game.ourScore + ourTakenPoints + ourBetPoints,
    theirScore: game.theirScore + theirTakenPoints + theirBetPoints,
    history: [...game.history, turn],
  };
}

/**
 * @param turn {Pick<Turn, "takenPoints" | "consecutive">}
 * @returns {{us: number, them: number}}
 */
export function calculateTakenPoints(turn) {
  switch (turn.consecutive) {
    case "none":
      return {
        us: turn.takenPoints,
        them: 100 - turn.takenPoints,
      };
    case "us":
      return {
        us: 200,
        them: 0,
      };
    case "them":
      return {
        us: 0,
        them: 200,
      };
  }
}

/**
 * @param turn {Turn}
 * @returns {{us: number, them: number}}
 */
function turnBetPoints(turn) {
  return {
    us: pointsForBets(turn.ourBets),
    them: pointsForBets(turn.theirBets),
  };
}

/**
 * @param bets {Bet[]}
 * @returns {number}
 */
function pointsForBets(bets) {
  return bets.reduce(function (sum, bet) {
    const sign = bet.successful ? 1 : -1;
    const value = bet.level === "grand" ? 200 : bet.level === "tichu" ? 100 : 0;

    return sum + sign * value;
  }, 0);
}

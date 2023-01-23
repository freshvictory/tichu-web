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
 * Given a list of turns, return a `Game` with
 * the current scores.
 *
 * @param {Turn[]} turns
 * @returns {Game}
 */
export function gameFromTurns(turns) {
  const { us, them } = turns.reduce(
    function (scores, turn) {
      const turnScore = scoreTurn(turn);

      return {
        us: scores.us + turnScore.us,
        them: scores.them + turnScore.them,
      };
    },
    { us: 0, them: 0 }
  );

  return {
    ourScore: us,
    theirScore: them,
    history: turns,
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
 * Calculate the points for each team given a `Turn`.
 *
 * @param {Turn} turn
 * @returns {{ us: number, them: number }}
 */
function scoreTurn(turn) {
  const { us: ourTakenPoints, them: theirTakenPoints } =
    calculateTakenPoints(turn);

  const { us: ourBetPoints, them: theirBetPoints } = turnBetPoints(turn);

  return {
    us: ourTakenPoints + ourBetPoints,
    them: theirTakenPoints + theirBetPoints,
  };
}

/**
 * Undo a turn.
 *
 * @param {Game} game
 *
 * @returns {Game}
 */
export function undo(game) {
  const lastTurn = game.history.at(-1);

  if (!lastTurn) {
    return game;
  }

  const { us, them } = scoreTurn(lastTurn);

  return {
    ourScore: game.ourScore - us,
    theirScore: game.theirScore - them,
    history: game.history.slice(0, -1),
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
    if (bet === "none") {
      return sum;
    }

    const sign = bet.successful ? 1 : -1;
    const value = bet.level === "grand" ? 200 : bet.level === "tichu" ? 100 : 0;

    return sum + sign * value;
  }, 0);
}

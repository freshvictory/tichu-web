/*
 * Form elements
 */
const slider = /** @type {HTMLInputElement} */ (
  document.getElementById("taken-points")
);
const sliderOutput = document.getElementById("taken-output");
const sliderOutputUs = /** @type {HTMLOutputElement} */ (
  document.getElementById("taken-us")
);
const sliderOutputThem = /** @type {HTMLOutputElement} */ (
  document.getElementById("taken-them")
);

const consecutiveUs = /** @type {HTMLInputElement} */ (
  document.getElementById("consecutive-us")
);
const consecutiveThem = /** @type {HTMLInputElement}} */ (
  document.getElementById("consecutive-them")
);
const form = /** @type {HTMLFormElement} */ (
  document.getElementById("score-form")
);

form.addEventListener("input", function () {
  const data = new FormData(form);
  const state = formState(data);

  const takenPoints = calculateTakenPoints(state);

  renderTakenPoints(takenPoints);
});

function renderTakenPoints({ us, them }) {
  sliderOutputUs.value = us;
  sliderOutputThem.value = them;
  sliderOutput.style.setProperty("--value", us);
}

/*
 * Form
 */
const topTeamOutput = /** @type {HTMLOutputElement} */ (
  document.getElementById("top-score")
);
const bottomTeamOutput = /** @type {HTMLOutputElement} */ (
  document.getElementById("bottom-score")
);

/**
 * @typedef {{
 *  takenPoints: number;
 *  consecutive: Consecutive;
 * }} FormState
 */

/**
 * @param data {FormData}
 * @returns {FormState}
 */
function formState(data) {
  const takenPointsInput = data.get("takenPoints");
  const takenPoints =
    typeof takenPointsInput === "string" && parseInt(takenPointsInput, 10);

  const consecutive = /** @type {Consecutive} */ (data.get("consecutive"));

  return {
    takenPoints,
    consecutive,
  };
}

form.addEventListener("submit", function (event) {
  event.preventDefault();

  const data = new FormData(form);
  const { takenPoints, consecutive } = formState(data);

  const game = newGame();
  const next = score(
    {
      consecutive,
      ourBets: ["none", "none"],
      theirBets: ["none", "none"],
      takenPoints,
    },
    game
  );

  form.reset();

  renderGame(next);
});

/**
 * @param game {Game}
 */
function renderGame(game) {
  topTeamOutput.value = game.ourScore.toString();
  bottomTeamOutput.value = game.theirScore.toString();
}

/**
 * @typedef {"tichu" | "grand"} BetLevel
 *
 * @typedef {"none" | "us" | "them"} Consecutive
 *
 * @typedef {"none" | {
 *   level: BetLevel;
 *   successful: boolean;
 * }}
 * Bet
 *
 * @typedef {{
 *    takenPoints: number;
 *    consecutive: Consecutive;
 *    ourBets: [Bet, Bet];
 *    theirBets: [Bet, Bet];
 * }}
 * Turn
 *
 * @typedef {{
 *    ourScore: number;
 *    theirScore: number;
 *    history: Turn[];
 * }}
 * Game
 */

/**
 * @returns {Game}
 */
function newGame() {
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
function score(turn, game) {
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
function calculateTakenPoints(turn) {
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

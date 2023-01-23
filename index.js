import {
  calculateTakenPoints,
  gameFromTurns,
  newGame,
  score,
  undo,
} from "./tichu.js";

/**
 * @param {Elements} elements
 */
export function attach(elements) {
  elements.form.addEventListener("input", onFormInput.bind(null, elements));
  elements.form.addEventListener(
    "submit",
    onFormSubmit.bind(null, elements.form)
  );
}

/**
 * @param {Elements} elements
 */
function onFormInput(elements) {
  const data = new FormData(elements.form);
  const state = formStateToTurn(data);

  const takenPoints = calculateTakenPoints(state);

  renderTakenPoints(elements, takenPoints);
}

/**
 *
 * @param {Pick<Elements, keyof Elements & `slider${string}`>} elements
 * @param {{ us: number, them: number }} _
 */
function renderTakenPoints({ sliderOutputUs, sliderOutputThem }, { us, them }) {
  sliderOutputUs.value = numberToString(us);
  sliderOutputThem.value = numberToString(them);
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
const turnRecord = /** @type {HTMLElement} */ (
  document.getElementById("turn-record")
);

/**
 * Parse a `FormData` object into a `Turn`.
 *
 * @param {FormData} data
 * @returns {Turn}
 */
function formStateToTurn(data) {
  const takenPointsInput = /** @type {string} */ (data.get("takenPoints"));
  const takenPoints = parseInt(takenPointsInput, 10);

  const consecutive = /** @type {Consecutive} */ (data.get("consecutive"));

  const ourBets = /** @type {BetLevel[]} */ (data.getAll("ourBets"));
  const theirBets = /** @type {BetLevel[]} */ (data.getAll("theirBets"));
  const betSuccess = /** @type {string} */ (data.get("firstBetSuccess"));

  return {
    takenPoints,
    consecutive,
    ourBets: mapBetData("us", ourBets, [betSuccess]),
    theirBets: mapBetData("them", theirBets, [betSuccess]),
  };
}

/**
 * @param {"us" | "them"} team
 * @param {BetLevel[]} levels
 * @param {string[]} successes
 *
 * @returns {[Bet, Bet]}
 */
function mapBetData(team, levels, successes) {
  return [
    mapBet(team, levels[0], successes[0]),
    mapBet(team, levels[1], successes[1]),
  ];
}

/**
 * @param {"us" | "them"} team
 * @param {BetLevel} level
 * @param {string} success
 *
 * @returns {Bet}
 */
function mapBet(team, level, success) {
  return level ? { level, successful: success === `${team}-true` } : "none";
}

/**
 * @param {HTMLFormElement} form
 * @param {SubmitEvent} event
 */
function onFormSubmit(form, event) {
  event.preventDefault();

  const data = new FormData(form);

  const turns = /** @type {Turn[]} */ (
    data.getAll("turns").map(function (turn) {
      if (typeof turn !== "string") {
        return;
      }

      return JSON.parse(turn);
    })
  );

  const game = gameFromTurns(turns);

  const intent = event.submitter?.getAttribute("value");

  /** @type {Game} */
  let next;

  switch (intent) {
    case "undo": {
      next = undoForm(game);
      break;
    }
    default: {
      next = scoreForm(game, data);
      break;
    }
  }

  form.reset();

  renderGame(next);
}

/**
 * @param {Game} game
 * @param {FormData} data
 *
 * @returns {Game}
 */
function scoreForm(game, data) {
  const turn = formStateToTurn(data);
  return score(turn, game);
}

/**
 * @param {Game} game
 *
 * @returns {Game}
 */
function undoForm(game) {
  return undo(game);
}

/**
 * @param {Game} game
 */
function renderGame(game) {
  topTeamOutput.value = numberToString(game.ourScore);
  bottomTeamOutput.value = numberToString(game.theirScore);

  recordTurns(game.history);
}

/**
 * @param {Turn[]} turns
 */
function recordTurns(turns) {
  turnRecord.innerHTML = "";

  const elements = turns.map(function (turn, index) {
    const turnElement = document.createElement("input");
    turnElement.id = `turn-${index + 1}`;
    turnElement.type = "hidden";
    turnElement.name = "turns";
    turnElement.value = JSON.stringify(turn);

    return turnElement;
  });

  turnRecord.append(...elements);
}

/**
 * Converts a number to a string.
 * Uses the minus '−' character (`U+2212`)
 * instead of hyphen '-' for the minus sign.
 * @param {number} number
 */
function numberToString(number) {
  return number.toString().replace("-", "\u2212");
}

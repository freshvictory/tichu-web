import { calculateTakenPoints, newGame, score } from "./tichu.js";

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
  const turn = formStateToTurn(data);

  const game = newGame();
  const next = score(turn, game);

  form.reset();

  renderGame(next);
}

/**
 * @param game {Game}
 */
function renderGame(game) {
  topTeamOutput.value = numberToString(game.ourScore);
  bottomTeamOutput.value = numberToString(game.theirScore);
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

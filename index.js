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
  const state = formState(data);

  const takenPoints = calculateTakenPoints(state);

  renderTakenPoints(elements, takenPoints);
}

/**
 *
 * @param {Pick<Elements, keyof Elements & `slider${string}`>} elements
 * @param {{ us: number, them: number }} _
 */
function renderTakenPoints(
  { sliderOutputUs, sliderOutputThem, sliderOutputContainer },
  { us, them }
) {
  sliderOutputUs.value = us.toString();
  sliderOutputThem.value = them.toString();
  sliderOutputContainer.style.setProperty("--value", us.toString());
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
 * @param {FormData} data
 * @returns {FormState}
 */
function formState(data) {
  const takenPointsInput = /** @type {string} */ (data.get("takenPoints"));
  const takenPoints = parseInt(takenPointsInput, 10);

  const consecutive = /** @type {Consecutive} */ (data.get("consecutive"));

  return {
    takenPoints,
    consecutive,
  };
}

/**
 * @param {HTMLFormElement} form
 * @param {SubmitEvent} event
 */
function onFormSubmit(form, event) {
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
}

/**
 * @param game {Game}
 */
function renderGame(game) {
  topTeamOutput.value = game.ourScore.toString();
  bottomTeamOutput.value = game.theirScore.toString();
}

import {
  calculateTakenPoints,
  gameFromTurns,
  newGame,
  score,
  undo,
} from "./tichu.js";

/**
 * @param {Elements} elements
 * @param {Storage | undefined} storage
 */
export function attach(elements, storage) {
  const game = storage ? load(storage) : null;
  if (game) {
    renderGame(elements, game);
  }

  elements.form.addEventListener("input", onFormInput.bind(null, elements));
  elements.form.addEventListener("submit", function (event) {
    event.preventDefault();

    try {
      const next = onFormSubmit(event);
      renderGame(elements, next);
      if (storage) {
        save(storage, next);
      }
    } catch (e) {
      if (storage) {
        save(storage, newGame());
      }
      crash(e instanceof Error && e.message);
    }
  });
}

/**
 * @param {Storage} storage
 */
function load(storage) {
  const storedGame = storage.getItem("game");

  return storedGame ? JSON.parse(storedGame) : null;
}

/**
 * @param {Storage} storage
 * @param {Game} game
 */
export function save(storage, game) {
  storage.setItem("game", JSON.stringify(game));
}

/**
 * @param {string | false} message
 */
function crash(message) {
  document.body.innerHTML = message || "";
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

  const ourBets = /** @type {(BetLevel | "none")[]} */ (data.getAll("ourBets"));
  const theirBets = /** @type {(BetLevel | "none")[]} */ (
    data.getAll("theirBets")
  );
  const betSuccess = /** @type {string[]} */ (data.getAll("betSuccess"));

  return {
    takenPoints,
    consecutive,
    ourBets: mapBetData("us", ourBets, betSuccess),
    theirBets: mapBetData("them", theirBets, betSuccess),
  };
}

/**
 * @param {"us" | "them"} team
 * @param {(BetLevel | "none")[]} levels
 * @param {string[]} successes
 *
 * @returns {[Bet, Bet]}
 */
function mapBetData(team, levels, successes) {
  return [
    mapBet(
      team,
      levels[0],
      successes.find((s) => s.startsWith(`${team}-1`)) || "false"
    ),
    mapBet(
      team,
      levels[1],
      successes.find((s) => s.startsWith(`${team}-2`)) || "false"
    ),
  ];
}

/**
 * @param {"us" | "them"} team
 * @param {BetLevel | "none"} level
 * @param {string} success
 *
 * @returns {Bet}
 */
function mapBet(team, level, success) {
  if (!level || level === "none") {
    return "none";
  }

  return level ? { level, successful: success.endsWith("true") } : "none";
}

/**
 * @param {SubmitEvent} event
 * @returns {Game}
 */
function onFormSubmit(event) {
  const form = /** @type {HTMLFormElement} */ (event.target);
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
    case "new": {
      next = startNewGame(game);
      break;
    }
    case "undo": {
      next = undoForm(game);
      break;
    }
    case "crash": {
      throw new Error("The app crashed :(");
    }
    default: {
      next = scoreForm(game, data);
      break;
    }
  }

  form.reset();

  return next;
}

/**
 * @param {Game} game
 * @returns {Game}
 */
function startNewGame(game) {
  return window.confirm("Are you sure you want to start a new game?")
    ? newGame()
    : game;
}

/**
 * @param {Game} game
 * @param {FormData} data
 *
 * @returns {Game}
 */
function scoreForm(game, data) {
  const turn = formStateToTurn(data);
  console.log(turn);
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
 * @param {Elements} elements
 * @param {Game} game
 */
function renderGame(elements, game) {
  elements.topTeamOutput.value = numberToString(game.ourScore);
  elements.bottomTeamOutput.value = numberToString(game.theirScore);

  if (Math.abs(game.theirScore - game.ourScore) > 400) {
    elements.crash.style.display = "block";
  } else {
    elements.crash.style.display = "none";
  }

  recordTurns(elements.turnRecord, game.history);
}

/**
 * @param {HTMLElement} turnRecord
 * @param {Turn[]} turns
 */
function recordTurns(turnRecord, turns) {
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
  return new Intl.NumberFormat().format(number).replace("-", "\u2212");
}

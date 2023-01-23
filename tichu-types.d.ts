type Consecutive = "none" | "us" | "them";

type BetLevel = "tichu" | "grand";

type Bet =
  | {
      level: BetLevel;
      successful: boolean;
    }
  | "none";

type Turn = {
  takenPoints: number;
  consecutive: Consecutive;
  ourBets: [Bet, Bet];
  theirBets: [Bet, Bet];
};

type Game = {
  ourScore: number;
  theirScore: number;
  history: Turn[];
};

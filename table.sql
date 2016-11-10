
CREATE TABLE IF NOT EXISTS teams (
  teamName  varchar(100) NOT NULL,
  teamid    integer  NOT NULL PRIMARY KEY,
  teamtwitter varchar(100),
  teamno    integer NOT NULL
);


CREATE TABLE IF NOT EXISTS scores (
    league      varchar(40) NOT NULL,
    gamedate    varchar(100) NOT NULL,
    team1       integer NOT NULL,
    team2       integer NOT NULL,
    score1 			integer,
    score2      integer,
		CONSTRAINT key PRIMARY KEY(league, gamedate, team1, team2, score1, score2),
    FOREIGN KEY (team1) REFERENCES teams(teamid),
    FOREIGN KEY (team2) REFERENCES teams(teamid)
);

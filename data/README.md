# Data

`data/public/` holds the de-identified analysis files used for the paper. They reproduce the
analysis data exactly (verified against the private files), so every result can be regenerated.

## De-identification

- **Participant IDs** (`S01`–`S30`) were assigned in random order and are unrelated to study
  record numbers, app IDs, or enrollment order. The key is not shared.
- **No dates or clock times.** Time is represented only by `study_day` (1 = first EMA day) and
  `beep` (1–5). Two derived fields replace response times for the lagged models: `time_order`
  (rank of each completed prompt in time within a participant) and `lag_ok` (the previous
  completed prompt was on the same day and no more than 6 hours earlier).
- **No demographics** (age, sex, gender, race, ethnicity, education, student or employment status),
  no free text, and no items not used in the analyses (e.g., self-harm behavior, positive affect).
- Only scheduled prompts are included; event-contingent surveys are excluded.

## `public/ema_long.csv` — one row per scheduled prompt (missed prompts included)

| column | description |
|---|---|
| `pid` | participant ID (`S01`–`S30`, random) |
| `study_day`, `beep` | EMA day (1 = first day) and prompt within day (1–5) |
| `prompt_type` | `scheduled` |
| `completed` | 1 = prompt answered |
| `si` | screening item: thought about hurting or killing oneself (since last night at prompt 1; in the past few hours at prompts 2–5); 0/1. Captures self-injurious thoughts broadly, including thoughts of nonsuicidal self-injury, not suicidal thoughts specifically |
| `intent_suicide_now` | "What is your intention to kill yourself in this moment?" 0 = None, 1 = A little, 2 = Moderate, 3 = A lot, 4 = Severe; asked only when `si` = 1. Primary outcome = `intent_suicide_now` > 0 |
| `na_sad`, `na_angry`, `na_afraid`, `na_ashamed`, `na_guilty` | negative affect in the past 10 minutes, 0–100 |
| `mal_rumination`, `mal_distraction`, `mal_avoidance`, `mal_suppression`, `mal_selfinvalidation` | maladaptive strategy used since last prompt, 0/1 |
| `adp_reappraisal`, `adp_acceptance`, `adp_problemsolving` | adaptive strategy used since last prompt, 0/1 |
| `time_order` | rank of the prompt in time order among the participant's completed prompts (NA if not completed) |
| `lag_ok` | 1 = previous completed prompt was the same day and ≤ 6 hours earlier |

## `public/person.csv` — one row per participant

| column | description |
|---|---|
| `pid` | participant ID |
| `cssrs_life_sev`, `cssrs_pm_sev` | C-SSRS ideation severity (0–5), lifetime and past month, baseline |
| `sb_life`, `sb_3mo` | any suicidal behavior, lifetime / past 3 months (0/1), C-SSRS |
| `connect`, `lonely` | NIH Toolbox Friendship and Loneliness raw scores |
| `dep`, `anx` | PROMIS Depression and Anxiety item banks, raw summed scores |
| `inq_pb`, `inq_tb` | Interpersonal Needs Questionnaire-15: perceived burdensomeness, thwarted belongingness |
| `fad` | Acquired Capability for Suicide Scale–Fearlessness About Death |
| `cerq_maladaptive`, `cerq_adaptive` | Cognitive Emotion Regulation Questionnaire composites |
| `*_post`, `*_fu` | the same measures at 6 weeks (post-treatment) and 12 weeks (follow-up) |

## Private files (study team only)

`R/00_import_redcap.R` and `R/00_import_inperson.R` build the private analysis files from the
REDCap exports in `data/raw/` (git-ignored). Run them and set `PRECISE_DATA=redcap` to use the
private files instead of `data/public/`.

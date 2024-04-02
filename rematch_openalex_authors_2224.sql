
/* The dataset includes 492,124 unique author_ids and 423,920 unique tweeter_ids forming 498,672 unique author-tweeter pairs. The file contains the following columns */
SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022`
/* 492,134 unique author_ids */
SELECT
    COUNT(DISTINCT(author_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022`

/* 423,920 unique tweeter_ids */
SELECT
    COUNT(DISTINCT(tweeter_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022`

/* Using the data from 2022 June, with which the dataset of tweeters was created, I extract the original names of the authors */
CREATE TABLE `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_names` AS
SELECT
    a.author_id as old_author_id,
    b.display_name,
    b.display_name_alternatives
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022` as a
    INNER JOIN `insyspo.openalex_2022_06.authors` as b ON a.author_id = b.id
GROUP BY
    a.author_id,
    b.display_name,
    b.display_name_alternatives

/* 492,134 unique author_ids */
SELECT * FROM `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_names`

/* From that same dataset, I select the works associated with each author */
CREATE TABLE `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` AS
SELECT
    c.*,
    d.doi
FROM
    (
        SELECT
            a.*,
            b.work_id
        FROM
            (
                SELECT
                    old_author_id,
                    display_name
                FROM
                    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_names`
                GROUP BY
                    old_author_id,
                    display_name
            ) as a
            INNER JOIN `insyspo.openalex_2022_06.works_authorships` as b ON a.old_author_id = b.author_id
    ) as c
    INNER JOIN `insyspo.openalex_2022_06.works` as d ON c.work_id = d.id
GROUP BY
    old_author_id,
    display_name,
    work_id,
    doi

/* 488,909 unique author_ids with publications */
SELECT
    COUNT(DISTINCT(old_author_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works`

/* 13,967,539 unique works */
SELECT
    COUNT(DISTINCT(work_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works`

/* 11,125,339 unique works (with DOI) */
SELECT
    COUNT(DISTINCT(work_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works`
WHERE
    doi IS NOT NULL

/* 19,818,997 relationships */
SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works`

/* 16,176,516 relationships (with DOI) */
SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works`
WHERE
    doi IS NOT NULL

/* Matching 2024 02 */
/* 19,472,781 relationships */
SELECT
    a.*
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` as a
    INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works` as b ON a.work_id = b.id

/* 13,695,327 unique works */
SELECT
    COUNT(DISTINCT(work_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` as a
    INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works` as b ON a.work_id = b.id

/* 16,165,282 relationships */
SELECT
    a.*
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` as a
    INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works` as b ON a.doi = b.doi

/* 11,113,671 unique works (with DOI) */
SELECT
    COUNT(DISTINCT(a.doi))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` as a
    INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works` as b ON a.doi = b.doi

/* Using the work_id and the DOIs, the works of the scholars from the 2022 dataset are connected with those from 2024 */
CREATE TABLE `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24` AS
SELECT
    *
FROM
    (
        SELECT
            a.*,
            b.doi as new_doi
        FROM
            `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` as a
            INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works` as b ON a.work_id = b.id
        GROUP BY
            a.old_author_id,
            a.display_name,
            a.work_id,
            a.doi,
            b.doi
        UNION
        ALL
        SELECT
            c.*,
            d.doi as new_doi
        FROM
            `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works` as c
            INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works` as d ON c.doi = d.doi
        GROUP BY
            c.old_author_id,
            c.display_name,
            c.work_id,
            c.doi,
            d.doi
    )
GROUP BY
    old_author_id,
    display_name,
    work_id,
    doi,
    new_doi

SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24`

/* Only 1511 works have different DOI */
SELECT
    COUNT(DISTINCT(work_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24`
WHERE
    doi != new_doi

/* 16,163,052 relationships */
SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24`
WHERE
    doi = new_doi

CREATE TABLE `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names_aux` AS
SELECT
    c.*,
    d.display_name as new_name,
    "" as display_name_alternative,
    0 as alternative
FROM
    (
        SELECT
            a.*,
            b.author_id
        FROM
            `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24` as a
            INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works_authorships` as b ON a.work_id = b.work_id
    ) as c
    INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.authors` as d ON c.author_id = d.id
WHERE
    LOWER(c.display_name) = LOWER(d.display_name)

CREATE TABLE `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names` AS
SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names_aux`
UNION
ALL
SELECT
    e.*,
    f.display_name_alternative,
    1 as alternative
FROM
    (
        SELECT
            c.*,
            d.display_name as new_name
        FROM
            (
                SELECT
                    a.*,
                    b.author_id
                FROM
                    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24` as a
                    INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.works_authorships` as b ON a.work_id = b.work_id
                WHERE
                    a.old_author_id NOT IN (
                        SELECT
                            old_author_id
                        FROM
                            `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names_aux`
                    )
            ) as c
            INNER JOIN `insyspo.publicdb_openalex_2024_02_rm.authors` as d ON c.author_id = d.id
    ) as e
    LEFT JOIN `insyspo.publicdb_openalex_2024_02_rm.authors_display_name_alternatives` as f ON e.author_id = f.author_id
WHERE
    LOWER(e.display_name) = LOWER(f.display_name_alternative)
GROUP BY
    old_author_id,
    display_name,
    work_id,
    doi,
    new_doi,
    author_id,
    new_name,
    display_name_alternative

DROP TABLE `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names_aux`

SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`

SELECT
    old_author_id,
    work_id,
    COUNT(*) as coun
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`
GROUP BY
    old_author_id,
    work_id

SELECT
    *
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`
WHERE
    old_author_id = 'https://openalex.org/A49930057'
    AND work_id = 'https://openalex.org/W2808930387'

/* 1688 need to be revised */
SELECT
    *
FROM
    (
        SELECT
            old_author_id,
            work_id,
            COUNT(*) as coun
        FROM
            (
                SELECT
                    old_author_id,
                    work_id,
                    author_id
                FROM
                    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`
                GROUP BY
                    old_author_id,
                    work_id,
                    author_id
            )
        GROUP BY
            old_author_id,
            work_id
    )
WHERE
    coun > 1

/* 432,417 rematched */
SELECT
    COUNT(DISTINCT(old_author_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`

/* 375,316 rematched (good) */
SELECT
    COUNT(DISTINCT(old_author_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`
WHERE
    alternative = 0

/* 57,101 rematched (alternatives) */
SELECT
    COUNT(DISTINCT(old_author_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`
WHERE
    alternative = 1

SELECT
    old_author_id,
    display_name as old_display_name,
    author_id,
    new_name,
    display_name_alternative,
    alternative
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names`
GROUP BY
    old_author_id,
    display_name,
    author_id,
    new_name,
    display_name_alternative,
    alternative

/* 388,968 unique tweeters */
SELECT
    COUNT(DISTINCT(tweeter_id))
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names` as a
    INNER JOIN `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022` as b ON a.old_author_id = b.author_id

/* 496,897 relationships */
SELECT
    tweeter_id,
    old_author_id,
    display_name as old_display_name,
    a.author_id,
    new_name,
    display_name_alternative,
    alternative
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names` as a
    INNER JOIN `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022` as b ON a.old_author_id = b.author_id
GROUP BY
    tweeter_id,
    old_author_id,
    display_name,
    a.author_id,
    new_name,
    display_name_alternative,
    alternative

/* 463,774 relationships */
SELECT
    tweeter_id,
    a.author_id,
    new_name,
    display_name_alternative,
    alternative
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names` as a
    INNER JOIN `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022` as b ON a.old_author_id = b.author_id
GROUP BY
    tweeter_id,
    a.author_id,
    new_name,
    display_name_alternative,
    alternative

/* First dataset: 462,427 relationships */
SELECT
    tweeter_id,
    a.author_id,
    alternative
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names` as a
    INNER JOIN `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022` as b ON a.old_author_id = b.author_id
GROUP BY
    tweeter_id,
    a.author_id,
    alternative

/* Second dataset: 496,897 relationships */
SELECT
    tweeter_id,
    old_author_id,
    display_name as old_display_name,
    a.author_id,
    new_name,
    display_name_alternative,
    alternative
FROM
    `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works_22_24_names` as a
    INNER JOIN `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022` as b ON a.old_author_id = b.author_id
GROUP BY
    tweeter_id,
    old_author_id,
    display_name,
    a.author_id,
    new_name,
    display_name_alternative,
    alternative

/* Third dataset: 19,818,997 relationships (limit is required to export more than 1GB in two parts) */
SELECT
    *
FROM
    (
        SELECT
            old_author_id,
            work_id,
            doi
        FROM
            `insyspo.userdb_wenceslao_arroyo.authors_tweeters_2022_works`
        GROUP BY
            old_author_id,
            work_id,
            doi
        ORDER BY
            old_author_id ASC
    )
LIMIT
    10000000

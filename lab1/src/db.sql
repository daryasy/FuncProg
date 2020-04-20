DROP TABLE IF EXISTS teachers;
CREATE TABLE teachers
(
    id      SERIAL PRIMARY KEY,
    name    VARCHAR(20) NOT NULL,
    surname VARCHAR(20) NOT NULL
);

DROP TABLE IF EXISTS sections;
CREATE TABLE sections
(
    id        SERIAL PRIMARY KEY,
    name      VARCHAR(20) NOT NULL,
    teacherID INT REFERENCES teachers (id)
);

DROP TABLE IF EXISTS students;
CREATE TABLE students
(
    id        SERIAL PRIMARY KEY,
    name      VARCHAR(20) NOT NULL,
    surname   VARCHAR(20) NOT NULL,
    sectionID INT REFERENCES sections (id)
);

DROP TABLE IF EXISTS schedule;
CREATE TABLE schedule
(
    id        SERIAL PRIMARY KEY,
    sectionID INT REFERENCES sections (id),
    beginDay  VARCHAR(3) NOT NULL,
    beginTime TIME       NOT NULL,
    endTime   TIME       NOT NULL
);

DROP TABLE IF EXISTS competitions;
CREATE TABLE competitions
(
    id        SERIAL PRIMARY KEY,
    sectionID INT REFERENCES sections (id),
    beginTime DATETIME NOT NULL,
    endTime   DATETIME NOT NULL
);

DROP TABLE IF EXISTS log_table;
CREATE TABLE log_table
(
    id             INT UNSIGNED                        NOT NULL AUTO_INCREMENT PRIMARY KEY,
    id_data        INT UNSIGNED                        NOT NULL,
    old_name       VARCHAR(20)                         NULL,
    new_name       VARCHAR(20)                         NULL,
    old_surname    VARCHAR(20)                         NULL,
    new_surname    VARCHAR(20)                         NULL,
    kind_of_change ENUM ('create', 'update', 'delete') NOT NULL,
    ts             TIMESTAMP                           NOT NULL
);

DELIMITER //

CREATE TRIGGER teachers_create
    AFTER INSERT
    ON teachers
    FOR EACH ROW
BEGIN
    INSERT INTO log_table
    (id_data, old_name, new_name,
     old_surname, new_surname,
     kind_of_change, ts)
    VALUES (new.id, NULL, new.name,
            NULL, new.surname, 'create', now());

END//

CREATE TRIGGER teachers_update
    AFTER UPDATE
    ON teachers
    FOR EACH ROW
BEGIN
    INSERT INTO log_table
    (id_data, old_name, new_name,
     old_surname, new_surname,
     kind_of_change, ts)
    VALUES (new.id, old.name, new.name,
            old.surname, new.surname, 'update', now());
END//

CREATE TRIGGER teachers_delete
    AFTER DELETE
    ON teachers
    FOR EACH ROW
BEGIN
    INSERT INTO log_table
    (id_data, old_name, new_name,
     old_surname, new_surname,
     kind_of_change, ts)
    VALUES (old.id, old.name, NULL,
            old.surname, NULL, 'delete', now());
END//

DELIMITER ;

INSERT INTO teachers (name, surname)
VALUES ('Oleg', 'Yashchuk');
INSERT INTO teachers (name, surname)
VALUES ('Danylo', 'Ostapenko');
INSERT INTO teachers (name, surname)
VALUES ('Oleksiy', 'Piskun');

INSERT INTO sections (name, teacherID)
VALUES ('Football', 1);
INSERT INTO sections (name, teacherID)
VALUES ('Swimming', 2);
INSERT INTO sections (name, teacherID)
VALUES ('Gym', 3);

INSERT INTO students (name, surname, sectionID)
VALUES ('Yevgeny', 'Varzar', 1);
INSERT INTO students (name, surname, sectionID)
VALUES ('Stanislav', 'Jus', 1);
INSERT INTO students (name, surname, sectionID)
VALUES ('Dmitry', 'Kudin', 2);
INSERT INTO students (name, surname, sectionID)
VALUES ('Ivan', 'Pogibko', 2);
INSERT INTO students (name, surname, sectionID)
VALUES ('Nikita', 'Popov', 3);
INSERT INTO students (name, surname, sectionID)
VALUES ('Vladislav', 'Reva', 3);

INSERT INTO schedule (sectionID, beginDay, beginTime, endTime)
VALUES (1, 'Mon', '16:00', '18:00');
INSERT INTO schedule (sectionID, beginDay, beginTime, endTime)
VALUES (1, 'Tue', '16:00', '18:00');
INSERT INTO schedule (sectionID, beginDay, beginTime, endTime)
VALUES (2, 'Wed', '16:00', '18:00');
INSERT INTO schedule (sectionID, beginDay, beginTime, endTime)
VALUES (2, 'Thu', '17:00', '19:00');
INSERT INTO schedule (sectionID, beginDay, beginTime, endTime)
VALUES (3, 'Fri', '18:00', '21:00');
INSERT INTO schedule (sectionID, beginDay, beginTime, endTime)
VALUES (3, 'Sat', '19:00', '21:00');

INSERT INTO competitions (sectionID, beginTime, endTime)
VALUES (1, '2020-11-04 14:00', '2020-11-04 20:00');
INSERT INTO competitions (sectionID, beginTime, endTime)
VALUES (2, '2020-11-05 16:00', '2020-11-04 20:00');
INSERT INTO competitions (sectionID, beginTime, endTime)
VALUES (3, '2020-11-07 12:00', '2020-11-04 20:00');

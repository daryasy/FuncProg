import           Data.Time
import           Database.MySQL.Base
import           Test.HUnit
import           Utils

tableNamesTest :: Test
tableNamesTest =
  test ["test" ~: "(tableNames)" ~: ["teachers", "sections", "students", "schedule", "competitions"] ~=? tableNames]

toNumTest :: Test
toNumTest = test ["test" ~: "(toNum \"14\")" ~: 14 ~=? toNum "14"]

toTimeTest :: Test
toTimeTest = test ["test" ~: "(toTime \"12:30\")" ~: TimeOfDay 12 30 0 ~=? toTime "12:30"]

toDateTest :: Test
toDateTest =
  test
    [ "test" ~: "(toDate \"2020-02-17 19:55\")" ~: LocalTime (fromGregorian 2020 02 17) (TimeOfDay 19 55 0) ~=?
      toDate "2020-02-17 19:55"
    ]

checkTableNameTest :: Test
checkTableNameTest =
  test
    [ "testTrue" ~: "(checkTableName \"sections\")" ~: True ~=? checkTableName "sections"
    , "testFalse" ~: "(checkTableName \"crocodiles\")" ~: False ~=? checkTableName "crocodiles"
    ]

tableColumnsTest :: Test
tableColumnsTest =
  test
    [ "testTeachers" ~: "(tableColumns Teachers)" ~: ["name", "surname"] ~=? tableColumns Teachers
    , "testSections" ~: "(tableColumns Sections)" ~: ["name", "teacherID"] ~=? tableColumns Sections
    , "testStudents" ~: "(tableColumns Students)" ~: ["name", "surname", "sectionID"] ~=? tableColumns Students
    , "testSchedule" ~: "(tableColumns Schedule)" ~: ["sectionID", "beginDay", "beginTime", "endTime"] ~=?
      tableColumns Schedule
    , "testCompetitions" ~: "(tableColumns Competitions)" ~: ["sectionID", "beginTime", "endTime"] ~=?
      tableColumns Competitions
    ]

updatableTableColumnsTest :: Test
updatableTableColumnsTest =
  test
    [ "testTeachers" ~: "(updatableTableColumns \"teachers\")" ~: ["name", "surname"] ~=?
      updatableTableColumns "teachers"
    , "testSections" ~: "(updatableTableColumns \"sections\")" ~: ["name", "teacherID"] ~=?
      updatableTableColumns "sections"
    , "testStudents" ~: "(updatableTableColumns \"students\")" ~: ["name", "surname", "sectionID"] ~=?
      updatableTableColumns "students"
    , "testSchedule" ~: "(updatableTableColumns \"schedule\")" ~: ["sectionID", "beginDay", "beginTime", "endTime"] ~=?
      updatableTableColumns "schedule"
    , "testCompetitions" ~: "(updatableTableColumns \"competitions\")" ~: ["sectionID", "beginTime", "endTime"] ~=?
      updatableTableColumns "competitions"
    , "testCrocodiles" ~: "(updatableTableColumns \"crocodiles\")" ~: [] ~=? updatableTableColumns "crocodiles"
    ]

checkUpdatableColumnsTest :: Test
checkUpdatableColumnsTest =
  test
    [ "testTeachers" ~: "(checkUpdatableColumns \"teachers\" \"name\")" ~: True ~=?
      checkUpdatableColumns "teachers" "name"
    , "testSections" ~: "(checkUpdatableColumns \"sections\" \"teacherID\")" ~: True ~=?
      checkUpdatableColumns "sections" "teacherID"
    , "testStudents" ~: "(checkUpdatableColumns \"students\" \"surname\")" ~: True ~=?
      checkUpdatableColumns "students" "surname"
    , "testSchedule" ~: "(checkUpdatableColumns \"schedule\" \"beginTime\")" ~: True ~=?
      checkUpdatableColumns "schedule" "beginTime"
    , "testCompetitions" ~: "(checkUpdatableColumns \"competitions\" \"endTime\")" ~: True ~=?
      checkUpdatableColumns "competitions" "endTime"
    , "testCrocodiles" ~: "(checkUpdatableColumns \"crocodiles\" \"color\")" ~: False ~=?
      checkUpdatableColumns "crocodiles" "color"
    ]

main :: IO Counts
main = do
  runTestTT tableNamesTest
  runTestTT toNumTest
  runTestTT toTimeTest
  runTestTT toDateTest
  runTestTT checkTableNameTest
  runTestTT tableColumnsTest
  runTestTT updatableTableColumnsTest
  runTestTT checkUpdatableColumnsTest

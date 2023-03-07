import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import UserMap
import TeamsAttendance

-- https://hspec.github.io/writing-specs.html

main :: IO ()
main = hspec $ do
  describe "UserMap" $ do
    describe "sorting by company then name" $ do
      it "returns sorted UserMaps as expected" $ do 
        ((sortByCompanyThenName userMaps) == expectedSortedUserMaps)
      it "returns sorted eMails in UserMaps as expected" $ do 
        (sortByCompanyThenName (userMapsFromEmailsOnly testEmails) == (sortByCompanyThenName expectedTestEmailsAsUserMaps))
      describe "adding just emails to an existing UserMap list" $ do
        it "returns a UserMap list with emails appended" $ do
          ((sortByCompanyThenName $ addUnknownEmailsToUserMaps testEmails userMaps) 
            == (sortByCompanyThenName $ userMaps ++ expectedTestEmailsAsUserMaps))


testEmails = [ "dirk@dlvm.be", "ria@dlvm.be", "anna@wintour.com" ]

expectedTestEmailsAsUserMaps = 
  [ UserMap {name = "dirk@dlvm.be", company="_Unknown", eMails=["dirk@dlvm.be"]}
  , UserMap {name = "ria@dlvm.be", company="_Unknown", eMails=["ria@dlvm.be"]}
  , UserMap {name = "anna@wintour.com", company="_Unknown", eMails=["anna@wintour.com"]}
  ]

userMaps = 
  [ UserMap {name = "Jan", company = "UN", eMails = ["jan.janssens@un.int","jan@abc.co.uk"]}
  , UserMap {name = "Simon", company = "Spain", eMails = ["simon@spain.es"]}
  , UserMap {name = "Pope", company = "Vatican", eMails = ["jacek@woityla.at"]}
  , UserMap {name = "Igor", company = "Spain", eMails = ["igor@sota.net", "igor@spain.es"]}
  ]

expectedSortedUserMaps = 
  [ UserMap {name = "Igor", company = "Spain", eMails = ["igor@sota.net", "igor@spain.es"]}
  , UserMap {name = "Simon", company = "Spain", eMails = ["simon@spain.es"]}
  , UserMap {name = "Jan", company = "UN", eMails = ["jan.janssens@un.int","jan@abc.co.uk"]}
  , UserMap {name = "Pope", company = "Vatican", eMails = ["jacek@woityla.at"]}
  ]


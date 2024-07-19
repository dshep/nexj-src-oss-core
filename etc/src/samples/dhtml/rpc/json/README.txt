WebApp Demo
Installation:
1) Move json.htm to a location in the model where it will be served by the server (under "web") (eg. core/meta/web)
2) Deploy the model to the server
3) Access json.htm at the path you deployed it (eg. http://localhost:8080/nexj/json.htm)

Creating a new user:
   Click new user,
   Fill in values in the table,
   Click create

   The page displays the OID for the new User

Reading Users
  Click read users

  * To retrieve all users and all attributes > click read
  * To set which attributes to retrieve > check the ones you want under Attributes > click read
  * To order by an attributes > Select an option from the OrderBy drop down >
      Select an option from the Order drop down > click read
  * To set a where condition > select condition and attribute, enter a value > click read

 **NOTE: Documentation on how to build a Request object can be obtained from the pop-up help in NexJ Studio when
         using scratch pad.
         Enter the text
            (User'read
         and wait for content assist to appear, or press the content assist key sequence.

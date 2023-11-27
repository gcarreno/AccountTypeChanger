# Account Type Changer

An application to facilitate the change of a series of accounts in order to keep them active.

When I suggested this little helper tool on the Discord server, someone suggested that the batch operations on the current wallet.

Since I'm always forgetting such details on applications I only use for some minutes while updating the blockchain, I decided to give it a try.

I was very disappointed to find that you can't use a single account to change the type of many others. The account itself must have the necessary 0.0001 Pascals to perform the account type change.

With this empirical knowledge, I'm back to my original idea and making it.

## How does it work?

### Connection to the wallet

1. Start your local instance of a wallet.
2. Make sure that the RPC server is enabled.
3. Start this application.
4. Fill in the necessary details to connect to the wallet.
5. Press the `Connect` button.

### Steps

Since the procedure I envision is to:

1. Send all the accounts one wants to change the amount of 0.0002
2. Wait a block so all the accounts have the funds.
3. Change all the accounts chosen in step 1 from type 0 to 1( using 0.0001 of the 0.0002 ).
4. Wait a block so all the accounts to have their type changed.
5. Change all the accounts chosen in step 1 from type 1 to 0( using the last 0.0001 of the 0.0002 ).

#### Step I

This is where one decides what accounts should be changed.

#### Step II

This is where one enacts the first change of two.

#### Step III

This is where one enacts the second change of two.

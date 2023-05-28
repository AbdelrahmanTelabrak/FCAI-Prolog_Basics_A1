friend(ahmed, samy).
friend(ahmed, fouad).
friend(samy, mohammed).
friend(samy, said).
friend(samy, omar).
friend(samy, abdullah).
friend(fouad, abdullah).
friend(abdullah, khaled).
friend(abdullah, ibrahim).
friend(abdullah, omar).
friend(mostafa, marwan).
friend(marwan, hassan).
friend(hassan, ali).

friend(hend, aisha).
friend(hend, mariam).
friend(hend, khadija).
friend(huda, mariam).
friend(huda, aisha).
friend(huda, lamia).
friend(mariam, hagar).
friend(mariam, zainab).
friend(aisha, zainab).
friend(lamia, zainab).
friend(zainab, rokaya).
friend(zainab, eman).
friend(eman, laila).

%task 1-----------------
is_friend(X, Y) :-
    friend(X, Y) ; friend(Y, X).
%task 1-----------------




%task 2----------------
friendList(X, L) :-
    find_friends(X, [], L).

% to recursively find all friends
find_friends(Person, List, Friends) :-
    is_friend(Person, Friend),
    in_List(Friend, List),
    find_friends(Person, [Friend|List], RestOfFriends),
    addToList(Friend, RestOfFriends, Friends).
find_friends(_, _, []).

% to check if an element is not in a list
in_List(_, []).
in_List(X, [H|T]) :- X \= H, in_List(X, T).

% implementation of append function
addToList(X, L, [X|L]).
%task 2----------------



%task 3-----------------
friendListCount(X,N):-
    friendList(X, L),
    list_size(L, N).


list_size(List, Size) :-
    list_size_acc(List, 0, Size).

list_size_acc([], Acc, Acc).
list_size_acc([_|T], Acc, Size) :-
    NewAcc is Acc + 1,
    list_size_acc(T, NewAcc, Size).
%task 3-----------------


%task 4------------------
peopleYouMayKnoww(X,Y):-
  is_friend(X, Z), % X has at least one mutual friend with Y
  is_friend(Z, Y), % Z is a mutual friend of X and Y
  \+ is_friend(X, Y), % Y is not already a friend of X
  X \= Y.


%task 4-------------------



%Task 5-------------------------


%The first clause of findall uses the bagof/3 predicate to collect all solutions of the Goalquery into the List variable. If bagof/3 fails to find any solutions, ! cuts off any further backtracking and prevents findall from returning multiple solutions.

find_all(X, Goal, List) :-
    bagof(X, Goal, List), !.
find_all(_, _, []).

%task 5:-----------------------
peopleYouMayKnow(Person, N, SuggestedFriend) :-
    find_all(Friend, friend(Person, Friend), Friends),
    find_all(MutualFriend,
        (member_I(Friend, Friends),
        friend(Friend, MutualFriend),
        \+ friend(Person, MutualFriend),
        \+ member_I(MutualFriend, Friends)),
        MutualFriends),
    sort(MutualFriends, SortedMutualFriends),
    find_all(SuggestedFriend,
        (member_I(SuggestedFriend, SortedMutualFriends),
        aggregate_all(count,
            (member(Friend, Friends), friend(Friend, SuggestedFriend)),
            Count),
        Count >= N),
        SuggestedFriends),
    member_I(SuggestedFriend, SuggestedFriends).

mutual_friends(X, Y) :-
  sfriend(X, [], Y).

sfriend(X, List, Friends, AllFriends):-
  is_friend(X, Z), % X has at least one mutual friend with Y
  is_friend(Z, SugFriend), % Z is a mutual friend of X and Y
  \+ is_friend(X, SugFriend), % Y is not already a friend of X
  X \= SugFriend,
  in_List(SugFriend, List),
  sfriend(X, [SugFriend|List], RestOfFriends),
  addToList(SugFriend, RestOfFriends, Friends).

sfriend(_,_,[]).
% to check if an element is not in a list
in_List(_, []).
in_List(X, [H|T]) :- X \= H, in_List(X, T).

% implementation of append function
addToList(X, L, [X|L]).

%Function That allows you to check whether an element is a member of a list. Here is an implementation of member:
member_I(X, [X|_]).
member_I(X, [_|T]) :- member_I(X, T).


%task 5-------------------------



% task 6----------------------------------------------------------------------
list_to_sett(List, Set) :-
    list_to_sett(List, [], Set).

list_to_sett([], Set, Set).
list_to_sett([Head|Tail], Acc, Set) :-
    (   member_I(Head, Acc)
    ->  list_to_sett(Tail, Acc, Set)
    ;   list_to_sett(Tail, [Head|Acc], Set)
    ).


friendsOfFriends(Person, FriendsOfFriends) :-
    find_all(FriendOfFriend, (is_friend(Person, Friend), is_friend(Friend, FriendOfFriend),(Person\=FriendOfFriend)), FriendsOfFriendsUnfiltered),

    list_to_sett(FriendsOfFriendsUnfiltered, FriendsOfFriends).
% task 6----------------------------------------------------------------------
















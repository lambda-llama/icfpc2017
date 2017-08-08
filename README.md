# icfpc2017
λ-llama code for ICFP contest 2017 

Team members:
  - Aleksey Kladov
  - Andrii Korol
  - Dmitry Polyanitsa
  - Eugene Kharitonov
  - Sergei Lebedev

## Post mortem

### Preparation

After selecting (after much debate) this year's preferred language we created a dummy app with a test suite and ensured that this bare minimum could be run by every team member. However, only during the contest did we realize that the version of .NET Core that we picked was not present on the judges' machine, so we needed to spend some time to upgrade.

We haven't prepared any code beforehand, which led to implementing (and re-implementing, read below) key data structures during the contest. We think that next time we might implement some of the most common data structures and general algorithms before the contest to not spend the same amount of time on that as we spent this year.

Almost everybody had a good level of familiarity with F# (either directly, or indirectly, through OCaml or the .NET framework), yet still we have been discovering new things we can do as we progressed through the contest. It's hard to measure, but it felt like the amount of infamiliarity was just right for most people, which made the contest coding both interesting and reasonably comfortable for everyone.

We also made sure we have a chatroom and tested that the videoconferencing works for all of us (the team members are based in different cities).

Key takeaways:
  - Carefully read all the pre-contest info to not spend time on fixing technical problems during the contest.
  - Decide if implementing some general data structures or algorithms makes sense. It might serve as a warm up, or it might lead to a burnout later.
  - Make sure team members are comfortable with the selected language.

### Day 1

(day here and below is 24 hours, 12GMT to 12GMT)

The [problem](https://icfpcontest2017.github.io/problem/) description practically dictated the data structure to be used, so to save time we've almost immediately started working in parallel on several things: graph and its visualization, protocol serialization and the tcp client. We were really rushing to start seeing the results fast (so as to participate in the lightning division) and didn't care too much for how extensible and even performant our code was, which will be something we'll later regret. We also didn't write a lot of  tests, thinking that with only one thing to do the program's behavior will make the errors apparent, which was partly true, but also led to some issues that were really difficult to diagnose.

We spent some time during the first day trying to implement the game server, which required adding a lot of serialization code and quickly proved to be pointless as the official servers were freely available. This decision was later strenghtened when we learned about the extensions (and the need to support them in the server if we wanted to maintain it). Later we will decide to implement a local simulation run instead, which will prove to be a good way to test our changes to the strategies quickly.

About 8 hours in, we had a good enough working strategy for the first day and the rest of the code worked reliably. However, we didn't pay too much attention to the "offline work" requirement for the contest submissions. In short, every application will be started and then killed for each its turn, so it must serialize all the state it needs as a custom JSON field in the official protocol. Moreover, any application that'll take more than 1 second for the turn will be forcibly killed and on the next turn it'll be given extra info to indicate that it timed out. This differed from the "online" mode where the timeouts were much more lenient and the app could run continuously, enjoying the persisted state. The organizers had only provided the servers for the "online" mode, which led to us attempting the implementation of the adapters during the day. Almost half a day in, they released their version of the adapter, which we needed to use to make our code submission-ready. It was 1-2am for us, so we skipped doing that to the next day.

That day we also added some more (better) strategies and spent some time writing the deployment script (we needed to use Docker and generate the submission archive in a particular way). Despite our efforts with the "offline mode" and creating good strategies for the lightning submission, we will later find out that the build was faulty, mostly because we didn't spend enough time making sure it matches all the requirements and due to a lack of careful planning.

Key takeaways:
  - Spend some time on planning before starting to code. Even the lightning contest is full 24 hours and by spending some time right away, whole classes of issues can be eliminated.
  - Carefully think whether what you implement will be needed and whether it's quick and easy to code and maintain.
  - Start with making your app submission-ready and then work from that. Adding state save/load and implementing a watchdog for keeping the strategies' running time under one second proved to be much more difficult compared to starting with these things not only in mind, but also in the code, guiding the rest of the implementation.

### Day 2

Around the early post-lightning time we started noticing the shortcomings of our graph API so we started a new one in parallel (also without writing any tests). While overall better, the new API will take a long time to be polished enough to cover all the use cases for the old one. We'll make a switch at around 6 hours after the start of day 2, which will be followed by a dozen of "fix" commits and then some ("	
fix the simplify of the fix of the fix of the fix").

Being busy with the basic stuff not working, we were not even hoping to start working on the extensions to the core game, which have been continuously added by the organizers (an extension per day). However, we create more ways to test our strategies, which will help in making them better and inventing the new heuristics. And with the new graph firmly in place, we finally started working on reducing the amount of time needed per turn and adding first attempts at timeout mitigation - running two strategies in parallel and allowing the quicker, sloppier one make the move if the good one is not making it.

Key takeaways:
  - Make sure that the shared code (such as data structures) is clean and of high quality. You can be more sloppy with your personal strategy or a helper tool.
  - Write tests for complex code where it's easy to make mistakes.
  - Use types better - we had a lot of issues with both external and internal indices being just "int".
  - Right after the lightning submission, think about what needs to be done in day 2 to be more productive during day 3, where it matters the most.

### Day 3

We were finally onto something with a brand new strategy that proved itself very capable in defeating both bots and other teams' algorithms alike. We will spend most of the day thinking on how to improve it further and then how to pick between it and its altered version (we will decide to flip a coin each game).

We still don't think too much about making strategies time out wisely and reconfigure themselves, yet mostly because our new strategy works fast and is not very easy to reconfigure. However, we still mostly ignore the game extensions, thinking that they are not very important and that most people will also focus on the main strategies. This changed with the third and final extension which made both previous ones matter a lot more and also was difficult to ignore from the implementation side.

The third extension was released around 10th hour of day 3, which was close to midnight for us and half of the team could not continue because of work the next day. The stronger part of the team kept on dealing with the new extension through the night, and then made the final changes to our submission, which we all can be proud of.

Key takeaways:
  - Be prepared for changes, especially when the organizers warn about them since day one (which was the case).
  - Make sure the team is aligned on who's staying the last night - there must be enough resources for the final effort.
  - Where there's not enough time, concentrate on what you think is most pragmatic to do. We were not hoping to be #1 in the contest, so we decided to spend more time on the general strategy to try and score better against other people who didn't take advantage of the extensions.

### Organization

While we think that overall the organization of the event was decent, there were, however, some things that could have been done better, as was noted both by us and by other teams on the official IRC channel.

The game servers were freely available throughout the contest, but it was very difficult to find people to play with. And when you did, a server restart could ruin tens of minutes of waiting for the game score (granted, that only happened to us once and the organizers apologized). Having matchmaking of some kind would have helped people to more easily and quickly find the opponents.

The "offline mode" adapter tool was released too late into the contest (we've already started writing ours) - there was no reason it could not have been available from the start. It was also written in a way which made running it on Windows impossible. In our team we had several people who used this operating system for a variety of reasons, which meant they could not run and test the "offline mode" on their machines.

The final extension release was too close to the end of contest, especially considering how impactful it was even if you didn't plan to take advantage of it. Doing that even a couple of hours earlier would have saved us some headache and allowed to not stay late into the night making it work.

That said, we think that the organizers did a great job coming up with an interesting task, making sure everyone can use their favorite tools, ensuring the test servers are always available and caring about the equal conditions for each submission, so people with more processing power could not have the unfair advantage. Great job, guys!

### Conclusion

We have spent three days doing what we like, solving interesting problems and generally having a good time. Despite the things that went not in the best way, we are pretty satisfied with what we have achieved. We've outlined things that we need to take into account for the ICFP contest 2018, which we hope we will.

\- lambda-llama team 2017

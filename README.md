# TypeScript High Performance
This is the code repository for [TypeScript High Performance](https://www.packtpub.com/application-development/typescript-high-performance?utm_source=github&utm_medium=repository&utm_campaign=9781785288647), published by [Packt](https://www.packtpub.com/?utm_source=github). It contains all the supporting project files necessary to work through the book from start to finish.
## About the Book
In a world where a tiny decrease in frames per second impacts customer engagement greatly, writing highly scalable code is more of a necessity than a luxury. Using TypeScript you get type checking during development. This gives you the power to write optimized code quickly. This book is also a solid tool to those who’re curious to understand the impact of performance in production, and it is of the greatest aid to the proactive developers who like to be cognizant of and avoid the classic pitfalls while coding.


## Instructions and Navigation
All of the code is organized into folders. Each folder starts with a number followed by the application name. For example, Chapter02.



The code will look like the following:
```
export enum FeedStrategy { Recent, Popular, MediaOnly };

    export interface IFeed { ... };

    export interface IFeedGenerator { ... };

    export function FeedGeneratorFactory(feedStrategy: FeedStrategy,  
    universalKey: string): IFeedGenerator { ... }
```

The book requires the following things:

Node and NPM
TypeScript
Mocha Test Framework
Chai Assertion Library
Windows, Linux, or MacOS
 Modern browsers--Chrome, Edge, and IE
Telerik Fiddler
Frontend editor--preferably VS Code or Atom/Sublime Text

## Related Products
* [Building Pro Web Apps with TypeScript 2.x [Video]](https://www.packtpub.com/application-development/building-pro-web-apps-typescript-2x-video?utm_source=github&utm_medium=repository&utm_campaign=9781788292054)

* [Rapid Web Application Development with TypeScript 2.x [Video]](https://www.packtpub.com/application-development/rapid-web-application-development-typescript-2x-video?utm_source=github&utm_medium=repository&utm_campaign=9781787287389)

* [Angular 2 Web Development with TypeScript [Video]](https://www.packtpub.com/web-development/angular-2-web-development-typescript-video?utm_source=github&utm_medium=repository&utm_campaign=9781785885563)

### Suggestions and Feedback
[Click here](https://docs.google.com/forms/d/e/1FAIpQLSe5qwunkGf6PUvzPirPDtuy1Du5Rlzew23UBp2S-P3wB-GcwQ/viewform) if you have any feedback or suggestions.

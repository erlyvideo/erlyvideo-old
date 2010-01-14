//
//  ErlyVideoPref.m
//  ErlyVideo
//
//  Created by sK0T on 13.01.10.
//  Copyright (c) 2010 __MyCompanyName__. All rights reserved.
//

#import "ErlyVideoPref.h"


@interface ErlyVideoPref(private)
- (void)updateServerStatus;
@end

@implementation ErlyVideoPref(private)
- (void)updateServerStatus
{
	[enableServer setEnabled:NO];
	[enableServer setState:NSOffState];
	[enableServer setEnabled:YES];
}
@end


@implementation ErlyVideoPref

- (void)mainViewDidLoad
{
	[self updateServerStatus];
}

- (IBAction)enableServerClicked:(NSButton *)sender
{
	[self updateServerStatus];
}

- (IBAction)help:(NSButton *)sender
{
}


@end

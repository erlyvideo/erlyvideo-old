//
//  ErlyVideo.m
//  ErlyVideo
//
//  Created by sK0T on 15.01.10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "ErlyVideo.h"


@implementation ErlyVideo

static BOOL FakeStatus = NO;
+ (NSArray *)searchPathes
{
	return [NSArray arrayWithObjects:@"~/Library/ErlyVideo",
			@"/Library/ErlyVideo", nil];
}

+ (BOOL)isActive
{
	return FakeStatus;
}

+ (BOOL)installed
{
	return NO;
}

+ (int)port
{
	return 1935;
}

+ (void)stop:(id<ErlyVideoClient>)sender
{
	FakeStatus = NO;
	[(NSObject *)sender performSelector:@selector(erlyVideoStatusChanged)
							 withObject:nil
							 afterDelay:1.0];
}

+ (void)start:(id<ErlyVideoClient>)sender
{
	FakeStatus = YES;
	[(NSObject *)sender performSelector:@selector(erlyVideoStatusChanged)
							 withObject:nil
							 afterDelay:1.0];
}

@end

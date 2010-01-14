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
- (void)reportNoErlyVideoFound;
@end

@implementation ErlyVideoPref(private)
- (void)updateServerStatus
{
	BOOL active = [ErlyVideo isActive];
	[enableServer setState:active?NSOnState:NSOffState];

	NSString *msg;
	if (active) {
		msg = NSLocalizedString(@"Flash-based players can access streaming video on this computer via RTMP on rtmp://%@%@/",
								@"Message format for active server");
		int port = [ErlyVideo port];
		msg = [NSString stringWithFormat:msg, [[NSHost currentHost] name],
			   (port != 1935)?[NSString stringWithFormat:@":%d", port]:@""];
	} else {
		msg = NSLocalizedString(@"ErlyVideo allows Flash-based players to access streaming video over the network.",
								@"General ErlyVideo description");
	}
	[hintLine setStringValue:msg];
}

- (void)reportNoErlyVideoFound
{
	[enableServer setEnabled:NO];
	NSString *msg = NSLocalizedString(@"ErlyVideo is not installed correctly, please check your setup.",
									  @"Message reporting about wrong ErlyVideo installation");
	[hintLine setStringValue:msg];
	[warningSign setHidden:NO];
}

@end


@implementation ErlyVideoPref

- (void)mainViewDidLoad
{
	if (![ErlyVideo installed]) {
		[self reportNoErlyVideoFound];
		return;
	}
	[self updateServerStatus];
}

- (IBAction)enableServerClicked:(NSButton *)sender
{
	[enableServer setEnabled:NO];
	if ([ErlyVideo isActive])
		[ErlyVideo stop:self];
	else
		[ErlyVideo start:self];
}

- (void)erlyVideoStatusChanged
{
	[self updateServerStatus];
	[enableServer setEnabled:YES];
}

- (IBAction)help:(NSButton *)sender
{
	[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:ERLY_VIDEO_HELP_URL]];
}

@end

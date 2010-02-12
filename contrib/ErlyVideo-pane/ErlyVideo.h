//
//  ErlyVideo.h
//  ErlyVideo
//
//  Created by sK0T on 15.01.10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@protocol ErlyVideoClient
// notify client about status change
- (void)erlyVideoStatusChanged;
@end

#define ERLY_VIDEO_HELP_URL	@"http://erlyvideo.org/help"

@interface ErlyVideo : NSObject {

}

/** Returns array of default pathes for ErlyVideo */
+ (NSArray *)searchPathes;

/** Returns YES if ErlyVideo is running and can be connected */
+ (BOOL)isActive;

/** Returns NO if ErlyVideo or Erlang is not found on default locations */
+ (BOOL)installed;

/** RTMP server port */
+ (int)port;

/** Stop ErlyVideo server */
+ (void)stop:(id<ErlyVideoClient>)sender;

/** Start ErlyVideo server */
+ (void)start:(id<ErlyVideoClient>)sender;

@end

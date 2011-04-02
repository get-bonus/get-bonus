/*
 * Copyright (c) 2007 Dave Dribin
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#import "WatcherWindowController.h"
#import "DDHidQueue.h"
#import "DDHidEvent.h"
#import "DDHidUsage.h"

@interface WatcherEvent : NSObject
{
    NSString * mUsageDescription;
    DDHidEvent * mEvent;
    int mIndex;
}

- (id) initWithUsageDescription: (NSString *) anUsageDecription 
                          event: (DDHidEvent *) anEvent
                          index: (int) index;

- (NSString *) usageDescription;
- (DDHidEvent *) event;
- (int) index;

@end

@implementation WatcherEvent : NSObject

- (id) initWithUsageDescription: (NSString *) anUsageDescription 
                          event: (DDHidEvent *) anEvent
                          index: (int) index
{
    if (self = [super init])
    {
        mUsageDescription = [anUsageDescription retain];
        mEvent = [anEvent retain];
        mIndex = index;
    }
    return self;
}

//=========================================================== 
// dealloc
//=========================================================== 
- (void) dealloc
{
    [mUsageDescription release];
    [mEvent release];
    
    mUsageDescription = nil;
    mEvent = nil;
    [super dealloc];
}

//=========================================================== 
// - usageDescription
//=========================================================== 
- (NSString *) usageDescription
{
    return mUsageDescription; 
}

//=========================================================== 
// - event
//=========================================================== 
- (DDHidEvent *) event
{
    return mEvent; 
}

//=========================================================== 
// - index
//=========================================================== 
- (int) index
{
    return mIndex;
}

@end

@implementation WatcherWindowController

- (id) init
{
    self = [super initWithWindowNibName: @"EventWatcher" owner: self];
    if (self == nil)
        return nil;
    
    mEventHistory = [[NSMutableArray alloc] init];
    mNextIndex = 1;
    
    return self;
}


//=========================================================== 
// dealloc
//=========================================================== 
- (void) dealloc
{
    [mQueue release];
    [mDevice release];
    [mElements release];
    [mEventHistory release];
    
    mQueue = nil;
    mDevice = nil;
    mElements = nil;
    mEventHistory = nil;
    [super dealloc];
}

- (void)windowWillClose:(NSNotification *)notification
{
    [mQueue release];
    mQueue = nil;
    [mDevice close];
    [self autorelease];
}

- (IBAction)showWindow:(id)sender
{
    [super showWindow: sender];
}

- (void) ddhidQueueHasEvents: (DDHidQueue *) hidQueue;
{
    WatcherEvent * watcherEvent;
    watcherEvent =
        [[WatcherEvent alloc] initWithUsageDescription: @"-----------------------------"
                                                 event: nil
                                                 index: mNextIndex++];
    [watcherEvent autorelease];
    [mEventHistoryController addObject: watcherEvent];

    NSMutableArray * newEvents = [NSMutableArray array];
    DDHidEvent * event;
    while (event = [hidQueue nextEvent])
    {
        DDHidElement * element = [mDevice elementForCookie: [event elementCookie]];
        watcherEvent =
            [[WatcherEvent alloc] initWithUsageDescription: [[element usage] usageNameWithIds]
                                                     event: event
                                                     index: mNextIndex++];
        [watcherEvent autorelease];
        [newEvents addObject: watcherEvent];
    }
    
    [mEventHistoryController addObjects: newEvents];
}

- (void) windowDidLoad;
{
    [mDevice open];
    mQueue = [[mDevice createQueueWithSize: 30] retain];
    [mQueue setDelegate: self];
    [mQueue addElements: mElements];
    [self willChangeValueForKey: @"watching"];
    [mQueue startOnCurrentRunLoop];
    [self didChangeValueForKey: @"watching"];
}

//=========================================================== 
//  device 
//=========================================================== 
- (DDHidDevice *) device
{
    return [[mDevice retain] autorelease]; 
}

- (void) setDevice: (DDHidDevice *) newDevice
{
    if (mDevice != newDevice)
    {
        [mDevice release];
        mDevice = [newDevice retain];
    }
}

//=========================================================== 
//  elements 
//=========================================================== 
- (NSArray *) elements
{
    return [[mElements retain] autorelease]; 
}

- (void) setElements: (NSArray *) newElements
{
    if (mElements != newElements)
    {
        [mElements release];
        mElements = [newElements retain];
    }
}

//=========================================================== 
//  eventHistory 
//=========================================================== 
- (NSMutableArray *) eventHistory
{
    return mEventHistory; 
}

- (void) setEventHistory: (NSMutableArray *) anEventHistory
{
    if (mEventHistory != anEventHistory)
    {
        [mEventHistory release];
        mEventHistory = [anEventHistory retain];
    }
}
- (void) addToEventHistory: (id)mEventHistoryObject
{
    [[self eventHistory] addObject: mEventHistoryObject];
}
- (void) removeFromEventHistory: (id)mEventHistoryObject
{
    [[self eventHistory] removeObject: mEventHistoryObject];
}

- (BOOL) isWatching;
{
    if (mQueue == nil)
        return NO;
    return [mQueue isStarted];
}

- (void) setWatching: (BOOL) watching;
{
    BOOL isStarted = [mQueue isStarted];
    if (isStarted == watching)
        return;
    
    if (watching)
        [mQueue startOnCurrentRunLoop];
    else
        [mQueue stop];
}

- (IBAction) clearHistory: (id) sender;
{
    [self willChangeValueForKey: @"eventHistory"];
    [mEventHistory removeAllObjects];
    mNextIndex = 1;
    [self didChangeValueForKey: @"eventHistory"];
}

@end

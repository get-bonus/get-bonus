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

#import <Cocoa/Cocoa.h>
#import "DDHidDevice.h"
#import "DDHidElement.h"

@class DDHidQueue;

@interface WatcherWindowController : NSWindowController
{
    IBOutlet NSArrayController * mEventHistoryController;
    DDHidDevice * mDevice;
    NSArray * mElements;
    DDHidQueue * mQueue;
    NSMutableArray * mEventHistory;
    int mNextIndex;
}

- (DDHidDevice *) device;
- (void) setDevice: (DDHidDevice *) newDevice;

- (NSArray *) elements;
- (void) setElements: (NSArray *) newElements;

- (NSMutableArray *) eventHistory;
- (void) setEventHistory: (NSMutableArray *) anEventHistory;
- (void) addToEventHistory: (id)mEventHistoryObject;
- (void) removeFromEventHistory: (id)mEventHistoryObject;

- (BOOL) isWatching;
- (void) setWatching: (BOOL) watching;

- (IBAction) clearHistory: (id) sender;

@end

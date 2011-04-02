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


@class DDHidKeyboard;

@interface KeyboardPaneController : NSObject
{
    IBOutlet NSArrayController * mKeyboardsController;
    IBOutlet NSArrayController * mKeyboardEventsController;

    NSArray * mKeyboards;
    unsigned mKeyboardIndex;
    NSMutableArray * mEvents;
    
    // Don't retain these
    DDHidKeyboard * mCurrentKeyboard;
}

- (NSArray *) keyboards;
- (void) setKeyboards: (NSArray *) theKeyboards;

- (unsigned) keyboardIndex;
- (void) setKeyboardIndex: (unsigned) theKeyboardIndex;

- (NSMutableArray *) events;
- (void) setEvents: (NSMutableArray *) theEvents;
- (void) addEvent: (id)theEvent;
- (void) removeEvent: (id)theEvent;

@end

@interface KeyboardPaneController (DDHidKeyboardDelegate)

- (void) ddhidKeyboard: (DDHidKeyboard *) keyboard
               keyDown: (unsigned) usageId;

- (void) ddhidKeyboard: (DDHidKeyboard *) keyboard
                 keyUp: (unsigned) usageId;

@end

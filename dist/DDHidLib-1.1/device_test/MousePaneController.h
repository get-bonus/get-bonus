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

@class DDHidQueue;
@class DDHidMouse;

@interface MousePaneController : NSObject
{
    IBOutlet NSArrayController * mMiceController;
    NSArray * mMice;
    DDHidMouse * mCurrentMouse;
    
    unsigned mMouseIndex;
    int mMouseX;
    int mMouseY;
    int mMouseWheel;
    NSMutableArray * mMouseButtons;
}

- (BOOL) no;

- (NSArray *) mice;
- (void) setMice: (NSArray *) newMice;

- (NSArray *) mouseButtons;

- (unsigned) mouseIndex;
- (void) setMouseIndex: (unsigned) theMouseIndex;

- (int) maxValue;

- (int) mouseX;

- (int) mouseY;

- (int) mouseWheel;

- (void) ddhidMouse: (DDHidMouse *) mouse xChanged: (SInt32) deltaX;
- (void) ddhidMouse: (DDHidMouse *) mouse yChanged: (SInt32) deltaY;
- (void) ddhidMouse: (DDHidMouse *) mouse wheelChanged: (SInt32) deltaWheel;

- (void) ddhidMouse: (DDHidMouse *) mouse buttonDown: (unsigned) buttonNumber;
- (void) ddhidMouse: (DDHidMouse *) mouse buttonUp: (unsigned) buttonNumber;

@end

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

#import "BoolFormatter.h"


@implementation BoolFormatter

- (id)initWithLables:(id)_l {
    if ((self = [super init]) != nil) {
        self->labels = [_l retain];
    }
    return self;
}

- (void)dealloc {
    [self->labels release];
    [super dealloc];
}

- (BOOL) boolForObjectValue: (id) object
{
    BOOL result;
    if ([object respondsToSelector: @selector(boolValue)])
        result = [object boolValue] ? YES : NO;
    else
        result = NO;
    return result;
}

- (NSString *)stringForObjectValue:(id)_obj
{
    NSString *str;
    
    if ([self boolForObjectValue: _obj])
        str = @"Yes";
    else
        str = @"No";

    return (self->labels != nil)
        ? (NSString *)[self->labels valueForKey:str] : str;
}

- (NSAttributedString *)attributedStringForObjectValue:(id)anObject
                                 withDefaultAttributes:(NSDictionary *)defaultAttributes
{
    NSDictionary * yesAttributes = [NSDictionary dictionaryWithObjectsAndKeys:
        [NSColor redColor], NSBackgroundColorAttributeName,
        nil];
    NSDictionary * attributes = defaultAttributes;
    if ([self boolForObjectValue: anObject])
        attributes = yesAttributes;
    
    NSAttributedString * string =
        [[NSAttributedString alloc] initWithString: [self stringForObjectValue: anObject]
                                        attributes: attributes];
    [string autorelease];
    return string;
}

@end

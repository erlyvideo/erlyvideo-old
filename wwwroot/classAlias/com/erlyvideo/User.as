package com.erlyvideo {
  import flash.net.registerClassAlias;
  public class User {
  registerClassAlias("com.erlyvideo.User", User);
  
  public var id : uint;
  public var name : String;
  public var age : uint;
  public function User() {
  }
  
  public function init(id:int, name:String, age:uint) : User {
    this.id = id;
    this.name = name;
    this.age = age;
    return this;
  }
  }
}

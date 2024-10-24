package com.autel.cloud.pile.base.domain.service;

public interface DelayService {

    /**
     * 生成一条随机的延时消息
     * @return
     */
    Boolean sendDelayMessage();

    /**
     * 使用SN生成延时消息
     * @return
     */
    Boolean sendDelayMessage(String sn);
}

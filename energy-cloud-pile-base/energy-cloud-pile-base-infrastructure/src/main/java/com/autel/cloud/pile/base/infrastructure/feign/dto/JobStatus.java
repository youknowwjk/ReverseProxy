package com.autel.cloud.pile.base.infrastructure.feign.dto;

/**
 * -4 请先为车辆设置标称电池容量 -3、超时终止  -2、网络失败   -1、连接车失败  1、尝试连接车辆  2、 连接车辆成功  3、尝试切换智能充电;   4、切换智能充电成功;  5、智能充电结束;  6、切换普通充电;
 */
public enum JobStatus {

    TERMINATIONS(-3),

    FAILURE_NETWORK(-2),

    FAILURE_VEHICLE(-1),

    QUICK(6),

    FINISH(5),

    RUNNING(4),

    TRY_SCHEDULE(3),

    LINK_SUCCESS(2),

    TRY_LINKED(1),

    NEW(0);

    JobStatus(int status) {
        this.status = status;
    }

    private int status;


    public int getStatus() {
        return status;
    }
}

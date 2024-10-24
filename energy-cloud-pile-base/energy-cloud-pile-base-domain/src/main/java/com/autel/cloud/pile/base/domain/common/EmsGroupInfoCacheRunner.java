package com.autel.cloud.pile.base.domain.common;

import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Slf4j
@Component
public class EmsGroupInfoCacheRunner implements CommandLineRunner {


    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @Override
    public void run(String... args) throws Exception {
        //异步更新加载数据
        new Thread(()->{
            try {
                opLocationPileGroupService.loadEmsGroupData();
            }catch (Exception e){
                log.info("加载ems类型的桩跟枪关系,出现异常：{}",e);
            }
        }).start();
    }
}

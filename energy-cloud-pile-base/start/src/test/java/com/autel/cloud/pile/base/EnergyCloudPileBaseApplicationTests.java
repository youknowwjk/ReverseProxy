package com.autel.cloud.pile.base;

import com.autel.cloud.pile.base.domain.model.ImminentExpireChargePointDTO;
import com.autel.cloud.pile.base.domain.service.EmailSendingService;
import com.autel.cloud.pile.base.enums.SubStatus;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

@SpringBootTest(classes = EnergyCloudPileBaseApplication.class)
@RunWith(SpringRunner.class)
@EnableAutoConfiguration
class EnergyCloudPileBaseApplicationTests {

    @Autowired
    EmailSendingService emailSendingService;

    @Test
    void test(){
        List<ImminentExpireChargePointDTO> list = new ArrayList<>();
        ImminentExpireChargePointDTO i = new ImminentExpireChargePointDTO();
        i.setExpireDate("2015-02-01");
        i.setGoodsName("Goods");
        i.setLocationName("Location");
        i.setName("Goods");
        i.setRemainDays(3l);
        i.setSubStatus(SubStatus.SOON_TO_EXPIRE);
        i.setSn("AAAAAAAA");
        list.add(i);
        emailSendingService.expirationReminderEmail(list, "http://localhost//xxxxxxxx.com", "aaaa", Locale.CHINA);
    }
}

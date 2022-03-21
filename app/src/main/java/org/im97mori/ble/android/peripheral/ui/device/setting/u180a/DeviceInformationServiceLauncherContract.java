package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class DeviceInformationServiceLauncherContract extends ActivityResultContract<byte[], byte[]> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, @Nullable byte[] input) {
        Intent intent = new Intent(context.getApplicationContext(), DeviceInformationServiceSettingActivity.class);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), input);
        return intent;
    }

    @Override
    public byte[] parseResult(int resultCode, @Nullable Intent intent) {
        byte[] data;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            data = intent.getByteArrayExtra(DEVICE_INFORMATION_SERVICE.toString());
        } else {
            data = null;
        }
        return data;
    }
}

package org.im97mori.ble.android.peripheral.ui.device.type;

import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class DeviceListLauncherContract extends ActivityResultContract<Void, Integer> {

    @NonNull
    @Override
    public Intent createIntent(@NonNull Context context, Void input) {
        return new Intent(context.getApplicationContext(), DeviceTypeListActivity.class);
    }

    @Override
    public Integer parseResult(int resultCode, @Nullable Intent intent) {
        Integer type;
        if (Activity.RESULT_OK == resultCode && intent != null) {
            type = intent.getIntExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED);
        } else {
            type = null;
        }
        return type;
    }
}

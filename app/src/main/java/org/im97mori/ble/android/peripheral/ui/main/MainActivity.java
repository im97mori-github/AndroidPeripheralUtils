package org.im97mori.ble.android.peripheral.ui.main;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.content.Intent;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.util.Pair;

import com.google.android.gms.oss.licenses.OssLicensesMenuActivity;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.MainActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.PeripheralActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceListLauncherContract;
import org.im97mori.ble.android.peripheral.utils.MockableViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import java.util.Collections;
import java.util.LinkedList;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class MainActivity extends AppCompatActivity {

    private MainViewModel mViewModel;

    private MainActivityBinding mBinding;

    private DeviceListAdapter adapter;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
    });

    private final ActivityResultLauncher<Void> mStartDeviceTypeListActivity = registerForActivityResult(new DeviceListLauncherContract(), result -> {
        if (result != null) {
            mStartDeviceSettingActivity.launch(Pair.create(VALUE_DEVICE_ID_UNSAVED, result));
        }
    });

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mViewModel = new MockableViewModelProvider(this).get(MainViewModel.class);
        mBinding = MainActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mBinding.grid.setEmptyView(mBinding.empty);
        adapter = new DeviceListAdapter(this, mViewModel.provideDeviceTypeImageResMap(), Collections.synchronizedList(new LinkedList<>()));
        mBinding.grid.setAdapter(adapter);
        mBinding.grid.setOnItemClickListener((parent, view, position, id) -> {
            Intent intent = new Intent(getApplicationContext(), PeripheralActivity.class);
            intent.putExtra(KEY_DEVICE_ID, adapter.getItemId(position));
            startActivity(intent);
        });

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeAllDeviceSetting(devices -> {
            adapter.setDeviceList(devices);
            mBinding.rootContainer.setVisibility(View.VISIBLE);
        }, throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

    @Override
    protected void onStop() {
        mViewModel.dispose();
        super.onStop();
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (R.id.create_device == item.getItemId()) {
            mStartDeviceTypeListActivity.launch(null);
            result = true;
        } else if (R.id.clear_devices == item.getItemId()) {
            mViewModel.observeDeleteAllDeviceSetting(() -> {
            }, throwable -> LogUtils.stackLog(throwable.getMessage()));
            result = true;
        } else if (R.id.license == item.getItemId()) {
            startActivity(new Intent(getApplicationContext(), OssLicensesMenuActivity.class));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}